package scair.passes.lower_refined_control_flow_to_llvm

import scair.MLContext
import scair.dialects.affine.*
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.dialects.func
import scair.ir.*
import scair.passes.control_flow_helpers.*
import scair.passes.lowering_helpers.FunctionLoweringState
import scair.transformations.*
import scair.transformations.patterns.*

import scala.collection.mutable

// This builder reconstructs refined structured control flow as LLVM CFG while
// explicitly threading the refined values that later lowering stages still use.
private final class Builder(val funcOp: func.Func):
  private val state = FunctionLoweringState(funcOp)
  private val blocks = mutable.ArrayBuffer.empty[Block]
  private val cfg = LoopCFGBuilder(blocks)
  private var current: Block = Block(funcOp.body.blocks.head.arguments.map(_.typ), Seq.empty)
  blocks += current
  state.blockMap(funcOp.body.blocks.head) = current
  state.valueMap.addAll(funcOp.body.blocks.head.arguments.zip(current.arguments))

  private def remap(v: Value[Attribute]): Value[Attribute] =
    state.remap(v)

  private def lowerBound(
      operands: Seq[Value[Attribute]],
      map: AffineMapAttr,
  ): Option[Value[Attribute]] =
    identityOrConstBound(operands.map(remap), map).map {
      case Left(k) =>
        cfg.emitIndexConstant(current, k)
      case Right(v) =>
        remap(v)
    }

  private def deepCopyOp(op: Operation): Operation =
    state.deepCopyOp(op)

  // Values defined at function entry remain available after loop lowering only
  // if they are threaded through block arguments explicitly.
  private def entryArgCaptures(ops: Seq[Operation]): Seq[Value[Attribute]] =
    val entryArgs = funcOp.body.blocks.head.arguments.toSet
    ops.flatMap(_.operands.map(_.asInstanceOf[Value[Attribute]])).filter(entryArgs.contains).distinct

  private def lowerSimpleOp(op: Operation): Unit =
    op match
      case nested: d_affine.For =>
        lowerFor(nested).foreach(v => state.valueMap(nested.res.head) = v)
      case other =>
        val copied = deepCopyOp(other)
        current.addOp(copied)
        state.valueMap.addAll(op.results.zip(copied.results))

  private def hasNestedLoopShape(op: d_affine.For): Boolean =
    if op.inits.size != 1 || op.res.size != 1 || op.body.blocks.size != 1 then false
    else
      op.body.blocks.head.operations.toSeq match
        case ops if ops.size >= 2 =>
          ops.last.isInstanceOf[d_affine.Yield] && ops(ops.size - 2).isInstanceOf[d_affine.For]
        case _ => false

  private def lowerNestedFor(op: d_affine.For): Option[Value[Attribute]] =
    val outerBody = op.body.blocks.head
    val outerOps = outerBody.operations.toSeq
    val prefixOps = outerOps.dropRight(2)
    val inner = outerOps(outerOps.size - 2).asInstanceOf[d_affine.For]
    val innerBody = inner.body.blocks.head
    for
      outerLb <- lowerBound(op.lowerBoundOperands, op.lowerBoundMap)
      outerUb <- lowerBound(op.upperBoundOperands, op.upperBoundMap)
      init = remap(op.inits.head)
      innerLb <- lowerBound(inner.lowerBoundOperands, inner.lowerBoundMap)
      innerUb <- lowerBound(inner.upperBoundOperands, inner.upperBoundMap)
    yield
      val captures = entryArgCaptures(prefixOps ++ innerBody.operations.toSeq)
      val outerHeader = Block(Seq(IndexType(), init.typ) ++ captures.map(_.typ), Seq.empty)
      cfg.appendBlock(outerHeader)
      val outerBodyEntry = Block(Seq(IndexType(), init.typ) ++ captures.map(_.typ), Seq.empty)
      cfg.appendBlock(outerBodyEntry)
      val prefixResultTypes = prefixOps.flatMap(_.results.map(_.typ))
      val innerHeader =
        Block(Seq(IndexType(), IndexType(), init.typ) ++ prefixResultTypes ++ captures.map(_.typ), Seq.empty)
      cfg.appendBlock(innerHeader)
      val innerBodyEntry =
        Block(Seq(IndexType(), IndexType(), init.typ) ++ prefixResultTypes ++ captures.map(_.typ), Seq.empty)
      cfg.appendBlock(innerBodyEntry)
      val outerLatch = Block(Seq(IndexType(), init.typ) ++ captures.map(_.typ), Seq.empty)
      cfg.appendBlock(outerLatch)
      val exit = Block(Seq(init.typ), Seq.empty)
      cfg.appendBlock(exit)

      // Refined lowering extends the baseline CFG skeleton by threading captured
      // layout-related values through block arguments.
      cfg.emitBr(current, Seq(outerLb, init) ++ captures.map(remap), outerHeader)

      val outerIv = outerHeader.arguments.head
      val outerAcc = outerHeader.arguments(1)
      val outerHeaderCaptures = outerHeader.arguments.drop(2)
      val outerCmp = cfg.emitICmpSlt(outerHeader, outerIv, outerUb)
      cfg.emitCondBr(
        outerHeader,
        outerCmp,
        Seq(outerIv, outerAcc) ++ outerHeaderCaptures,
        Seq(outerAcc),
        outerBodyEntry,
        exit,
      )

      val outerBodyIv = outerBodyEntry.arguments.head
      val outerBodyAcc = outerBodyEntry.arguments(1)
      val outerBodyCaptures = outerBodyEntry.arguments.drop(2)
      current = outerBodyEntry
      val savedOuter = mutable.Map.from(state.valueMap)
      state.valueMap.addAll(
        Seq(outerBody.arguments.head -> outerBodyIv, outerBody.arguments(1) -> outerBodyAcc) ++
          captures.zip(outerBodyCaptures)
      )
      prefixOps.foreach(lowerSimpleOp)
      val prefixValues = prefixOps.flatMap(_.results.map(r => remap(r)))
      state.valueMap.clear(); state.valueMap.addAll(savedOuter)
      cfg.emitBr(
        outerBodyEntry,
        Seq(outerBodyIv, innerLb, outerBodyAcc) ++ prefixValues ++ outerBodyCaptures,
        innerHeader,
      )

      val innerOuterIv = innerHeader.arguments.head
      val innerIv = innerHeader.arguments(1)
      val innerAcc = innerHeader.arguments(2)
      val innerPrefixExtras = innerHeader.arguments.drop(3).take(prefixResultTypes.size)
      val innerCaptures = innerHeader.arguments.drop(3 + prefixResultTypes.size)
      val innerCmp = cfg.emitICmpSlt(innerHeader, innerIv, innerUb)
      cfg.emitCondBr(
        innerHeader,
        innerCmp,
        Seq(innerOuterIv, innerIv, innerAcc) ++ innerPrefixExtras ++ innerCaptures,
        Seq(innerOuterIv, innerAcc) ++ innerCaptures,
        innerBodyEntry,
        outerLatch,
      )

      current = innerBodyEntry
      val saved = mutable.Map.from(state.valueMap)
      state.valueMap.addAll(
        Seq(
          op.body.blocks.head.arguments.head -> innerBodyEntry.arguments.head,
          op.body.blocks.head.arguments(1) -> outerBodyAcc,
          innerBody.arguments.head -> innerBodyEntry.arguments(1),
          innerBody.arguments(1) -> innerBodyEntry.arguments(2),
        ) ++ prefixOps.flatMap(_.results).zip(innerBodyEntry.arguments.drop(3).take(prefixResultTypes.size)) ++
          captures.zip(innerBodyEntry.arguments.drop(3 + prefixResultTypes.size))
      )
      var yielded: Option[Value[Attribute]] = None
      innerBody.operations.foreach {
        case y: d_affine.Yield => yielded = Some(remap(y.args.head))
        case other             => lowerSimpleOp(other)
      }
      state.valueMap.clear(); state.valueMap.addAll(saved)
      yielded.foreach { y =>
        val step = cfg.emitIndexConstant(current, inner.step.value.value)
        val nextIv = cfg.emitAdd(current, innerBodyEntry.arguments(1), step)
        cfg.emitBr(
          current,
          Seq(innerBodyEntry.arguments.head, nextIv, y) ++ innerBodyEntry.arguments.drop(3),
          innerHeader,
        )
      }

      current = outerLatch
      val outerLatchCaptures = outerLatch.arguments.drop(2)
      val outerStep = cfg.emitIndexConstant(current, op.step.value.value)
      val nextOuter = cfg.emitAdd(current, outerLatch.arguments.head, outerStep)
      cfg.emitBr(current, Seq(nextOuter, outerLatch.arguments(1)) ++ outerLatchCaptures, outerHeader)

      current = exit
      exit.arguments.head

  private def lowerFor(op: d_affine.For): Option[Value[Attribute]] =
    if hasNestedLoopShape(op) then lowerNestedFor(op)
    else if op.inits.size != 1 || op.res.size != 1 || op.body.blocks.size != 1 then None
    else
      for
        lb <- lowerBound(op.lowerBoundOperands, op.lowerBoundMap)
        ub <- lowerBound(op.upperBoundOperands, op.upperBoundMap)
      yield
        val init = remap(op.inits.head)
        val bodyBlock = op.body.blocks.head
        val header = Block(Seq(IndexType(), init.typ), Seq.empty)
        cfg.appendBlock(header)
        val body = Block(Seq(IndexType(), init.typ), Seq.empty)
        cfg.appendBlock(body)
        val exit = Block(Seq(init.typ), Seq.empty)
        cfg.appendBlock(exit)
        cfg.emitBr(current, Seq(lb, init), header)
        val iv = header.arguments.head
        val acc = header.arguments(1)
        val cmp = cfg.emitICmpSlt(header, iv, ub)
        cfg.emitCondBr(header, cmp, Seq(iv, acc), Seq(acc), body, exit)
        current = body
        val saved = mutable.Map.from(state.valueMap)
        state.valueMap.addAll(Seq(bodyBlock.arguments.head -> body.arguments.head, bodyBlock.arguments(1) -> body.arguments(1)))
        var yielded: Option[Value[Attribute]] = None
        bodyBlock.operations.foreach {
          case y: d_affine.Yield => yielded = Some(remap(y.args.head))
          case other             => lowerSimpleOp(other)
        }
        state.valueMap.clear(); state.valueMap.addAll(saved)
        yielded.foreach { y =>
          val step = cfg.emitIndexConstant(current, op.step.value.value)
          val nextIv = cfg.emitAdd(current, body.arguments.head, step)
          cfg.emitBr(current, Seq(nextIv, y), header)
        }
        current = exit
        exit.arguments.head

  def lower(): func.Func =
    funcOp.body.blocks.head.operations.foreach {
      case loop: d_affine.For =>
        lowerFor(loop).foreach(v => state.valueMap(loop.res.head) = v)
      case other =>
        lowerSimpleOp(other)
    }
    func.Func(funcOp.sym_name, funcOp.function_type, funcOp.sym_visibility, Region(blocks.toSeq))

private def lowerFunc(op: func.Func): Option[func.Func] =
  if op.body.blocks.size != 1 then None else Some(Builder(op).lower())

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists(_.isInstanceOf[d_affine.For])) =>
    lowerFunc(op).get
}

// Lowers refined affine control flow to explicit LLVM CFG.
// Example: `d_affine.for` / `d_affine.yield`
//   -> `llvm.br`, `llvm.cond_br`, and block arguments carrying loop state plus
//      captured refined values.
final class LowerRefinedControlFlowToLLVM(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "lower-refined-control-flow-to-llvm"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))

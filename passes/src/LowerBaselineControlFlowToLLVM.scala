package scair.passes.lower_baseline_control_flow_to_llvm

import scair.MLContext
import scair.dialects.affine.*
import scair.dialects.builtin.*
import scair.dialects.func
import scair.ir.*
import scair.passes.control_flow_helpers.*
import scair.passes.lowering_helpers.FunctionLoweringState
import scair.transformations.*
import scair.transformations.patterns.*

import scala.collection.mutable

// This builder reconstructs the function as explicit LLVM CFG because lowering
// affine loops requires coordinated block creation and SSA remapping.
private final class Builder(val funcOp: func.Func):
  private val state = FunctionLoweringState(funcOp)
  private val blocks = mutable.ArrayBuffer.empty[Block]
  private val cfg = LoopCFGBuilder(blocks)
  private var current: Block = Block(funcOp.body.blocks.head.arguments.map(_.typ), Seq.empty)
  blocks += current
  state.blockMap(funcOp.body.blocks.head) = current
  state.valueMap.addAll(funcOp.body.blocks.head.arguments.zip(current.arguments))

  private def emit(op: Operation): Unit = current.addOp(op)
  private def remap(v: Value[Attribute]): Value[Attribute] = state.remap(v)

  private def lowerBound(
      operands: Seq[Value[Attribute]],
      map: AffineMapAttr,
  ): Option[Value[Attribute]] =
    identityOrConstBound(operands.map(remap), map).map {
      case Left(k)  => cfg.emitIndexConstant(current, k)
      case Right(v) => remap(v)
    }

  private def deepCopyOp(op: Operation): Operation = state.deepCopyOp(op)

  private def lowerSimpleOp(op: Operation): Unit =
    op match
      case nested: For =>
        lowerFor(nested).foreach(v => state.valueMap(nested.results.head) = v)
      case other =>
        val copied = deepCopyOp(other)
        emit(copied)
        state.valueMap.addAll(op.results.zip(copied.results))

  private def hasNestedLoopShape(op: For): Boolean =
    if op.inits.size != 1 || op.res.size != 1 || op.body.blocks.size != 1 then false
    else
      op.body.blocks.head.operations.toSeq match
        case ops if ops.size >= 2 =>
          ops.last.isInstanceOf[Yield] && ops(ops.size - 2).isInstanceOf[For]
        case _ => false

  private def lowerNestedFor(op: For): Option[Value[Attribute]] =
    val outerBody = op.body.blocks.head
    val outerOps = outerBody.operations.toSeq
    val prefixOps = outerOps.dropRight(2)
    val inner = outerOps(outerOps.size - 2).asInstanceOf[For]
    val innerBody = inner.body.blocks.head
    for
      outerLb <- lowerBound(op.lowerBoundOperands, op.lowerBoundMap)
      outerUb <- lowerBound(op.upperBoundOperands, op.upperBoundMap)
      init = remap(op.inits.head)
      innerLb <- lowerBound(inner.lowerBoundOperands, inner.lowerBoundMap)
      innerUb <- lowerBound(inner.upperBoundOperands, inner.upperBoundMap)
    yield
      val outerHeader = Block(Seq(IndexType(), init.typ), Seq.empty)
      cfg.appendBlock(outerHeader)
      val outerBodyEntry = Block(Seq(IndexType(), init.typ), Seq.empty)
      cfg.appendBlock(outerBodyEntry)
      val prefixResultTypes = prefixOps.flatMap(_.results.map(_.typ))
      val innerHeader = Block(Seq(IndexType(), IndexType(), init.typ) ++ prefixResultTypes, Seq.empty)
      cfg.appendBlock(innerHeader)
      val innerBodyEntry = Block(Seq(IndexType(), IndexType(), init.typ) ++ prefixResultTypes, Seq.empty)
      cfg.appendBlock(innerBodyEntry)
      val outerLatch = Block(Seq(IndexType(), init.typ), Seq.empty)
      cfg.appendBlock(outerLatch)
      val exit = Block(Seq(init.typ), Seq.empty)
      cfg.appendBlock(exit)

      // The nested-loop lowering uses an explicit outer-header / inner-header /
      // latch structure so loop-carried values remain visible as block arguments.
      cfg.emitBr(current, Seq(outerLb, init), outerHeader)

      val outerIv = outerHeader.arguments.head
      val outerAcc = outerHeader.arguments(1)
      val outerCmp = cfg.emitICmpSlt(outerHeader, outerIv, outerUb)
      cfg.emitCondBr(
        outerHeader,
        outerCmp,
        Seq(outerIv, outerAcc),
        Seq(outerAcc),
        outerBodyEntry,
        exit,
      )

      val outerBodyIv = outerBodyEntry.arguments.head
      val outerBodyAcc = outerBodyEntry.arguments(1)
      current = outerBodyEntry
      val savedOuter = mutable.Map.from(state.valueMap)
      state.valueMap.addAll(Seq(outerBody.arguments.head -> outerBodyIv, outerBody.arguments(1) -> outerBodyAcc))
      prefixOps.foreach(lowerSimpleOp)
      val prefixValues = prefixOps.flatMap(_.results.map(r => remap(r)))
      state.valueMap.clear(); state.valueMap.addAll(savedOuter)
      cfg.emitBr(
        outerBodyEntry,
        Seq(outerBodyIv, innerLb, outerBodyAcc) ++ prefixValues,
        innerHeader,
      )

      val innerOuterIv = innerHeader.arguments.head
      val innerIv = innerHeader.arguments(1)
      val innerAcc = innerHeader.arguments(2)
      val innerExtras = innerHeader.arguments.drop(3)
      val innerCmp = cfg.emitICmpSlt(innerHeader, innerIv, innerUb)
      cfg.emitCondBr(
        innerHeader,
        innerCmp,
        Seq(innerOuterIv, innerIv, innerAcc) ++ innerExtras,
        Seq(innerOuterIv, innerAcc),
        innerBodyEntry,
        outerLatch,
      )

      current = innerBodyEntry
      val saved = mutable.Map.from(state.valueMap)
      state.valueMap.addAll(
        Seq(
          outerBody.arguments.head -> innerBodyEntry.arguments.head,
          outerBody.arguments(1) -> outerBodyAcc,
          innerBody.arguments.head -> innerBodyEntry.arguments(1),
          innerBody.arguments(1) -> innerBodyEntry.arguments(2),
        ) ++ prefixOps.flatMap(_.results).zip(innerBodyEntry.arguments.drop(3))
      )
      var yielded: Option[Value[Attribute]] = None
      innerBody.operations.foreach {
        case y: Yield => yielded = Some(remap(y.arguments.head))
        case other    => lowerSimpleOp(other)
      }
      state.valueMap.clear(); state.valueMap.addAll(saved)
      yielded.foreach { y =>
        val step = cfg.emitIndexConstant(current, op.step.value.value)
        val nextIv = cfg.emitAdd(current, innerBodyEntry.arguments(1), step)
        cfg.emitBr(
          current,
          Seq(innerBodyEntry.arguments.head, nextIv, y) ++ innerBodyEntry.arguments.drop(3),
          innerHeader,
        )
      }

      current = outerLatch
      val outerStep = cfg.emitIndexConstant(current, op.step.value.value)
      val nextOuter = cfg.emitAdd(current, outerLatch.arguments.head, outerStep)
      cfg.emitBr(current, Seq(nextOuter, outerLatch.arguments(1)), outerHeader)

      current = exit
      exit.arguments.head

  private def lowerFor(op: For): Option[Value[Attribute]] =
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
          case y: Yield => yielded = Some(remap(y.arguments.head))
          case other    => lowerSimpleOp(other)
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
      case loop: For => lowerFor(loop).foreach(v => state.valueMap(loop.results.head) = v)
      case other     => lowerSimpleOp(other)
    }
    func.Func(funcOp.sym_name, funcOp.function_type, funcOp.sym_visibility, Region(blocks.toSeq))

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists(_.isInstanceOf[For])) =>
    Builder(op).lower()
}

// Lowers baseline affine control flow to explicit LLVM CFG.
// Example: `affine.for` / `affine.yield`
//   -> `llvm.br`, `llvm.cond_br`, and block arguments carrying loop state.
final class LowerBaselineControlFlowToLLVM(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "lower-baseline-control-flow-to-llvm"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))

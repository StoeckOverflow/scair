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

  private def freshBlock(argumentTypes: Seq[Attribute]): Block =
    val block = Block(argumentTypes, Seq.empty)
    val depsBefore = mutable.ArrayBuffer.empty[(ValueAttribute, Value[Attribute])]
    block.arguments.foreach { arg =>
      AttributeWalker.foreachValueAttribute(arg.typ) { va =>
        depsBefore += ((va, va.getVal()))
      }
    }
    depsBefore.foreach { (va, v) =>
      v.typeUses -= TypeUse(block, va)
    }
    block.arguments.foreach(arg => AttributeWalker.remapTypeUsesInPlace(arg.typ)(using state.valueMap))
    block.arguments.foreach { arg =>
      AttributeWalker.foreachValueAttribute(arg.typ) { va =>
        va.getVal().typeUses += TypeUse(block, va)
      }
    }
    block

  private def lowerBoundWith(
      operands: Seq[Value[Attribute]],
      map: AffineMapAttr,
      remapper: Value[Attribute] => Value[Attribute],
      block: Block,
  ): Option[Value[Attribute]] =
    identityOrConstBound(operands.map(remapper), map).map {
      case Left(k)  => cfg.emitIndexConstant(block, k)
      case Right(v) => remapper(v)
    }

  private def collectExternalOperands(
      block: Block,
      parentDefs: Set[Value[Attribute]] = Set.empty,
  ): Seq[Value[Attribute]] =
    val seen = mutable.LinkedHashSet.empty[Value[Attribute]]

    def walkBlock(block: Block, incomingDefs: Set[Value[Attribute]]): Unit =
      var defs = incomingDefs ++ block.arguments.map(_.asInstanceOf[Value[Attribute]])
      block.operations.foreach { op =>
        op.operands.foreach { operand =>
          val value = operand.asInstanceOf[Value[Attribute]]
          if !defs.contains(value) then seen += value
        }
        op.regions.foreach(_.blocks.foreach(nested => walkBlock(nested, defs)))
        defs ++= op.results.map(_.asInstanceOf[Value[Attribute]])
      }

    walkBlock(block, parentDefs)
    seen.toSeq

  private def needsCapture(v: Value[Attribute]): Boolean =
    v.owner match
      case Some(block: Block) =>
        block ne funcOp.body.blocks.head
      case Some(op: Operation) =>
        op.containerBlock match
          case Some(block) => block ne funcOp.body.blocks.head
          case None        => false
      case _ => false

  private def lowerSimpleOp(
      op: Operation,
      continuationCaptures: Seq[Value[Attribute]] = Seq.empty,
  ): Unit =
    op match
      case nested: d_affine.For =>
        lowerLoop(nested, continuationCaptures)
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
    lowerBound(op.lowerBoundOperands, op.lowerBoundMap).map { outerLb =>
      val init = remap(op.inits.head)
      val captures =
        (
          op.lowerBoundOperands.map(_.asInstanceOf[Value[Attribute]]) ++
            op.upperBoundOperands.map(_.asInstanceOf[Value[Attribute]]) ++
            inner.lowerBoundOperands.map(_.asInstanceOf[Value[Attribute]]) ++
            inner.upperBoundOperands.map(_.asInstanceOf[Value[Attribute]]) ++
            collectExternalOperands(outerBody)
        ).distinct.filter(needsCapture)
      val outerHeader = freshBlock(Seq(IndexType(), init.typ) ++ captures.map(_.typ))
      cfg.appendBlock(outerHeader)
      val outerBodyEntry = freshBlock(Seq(IndexType(), init.typ) ++ captures.map(_.typ))
      cfg.appendBlock(outerBodyEntry)
      val prefixResultTypes = prefixOps.flatMap(_.results.map(_.typ))
      val innerHeader =
        freshBlock(Seq(IndexType(), IndexType(), init.typ) ++ prefixResultTypes ++ captures.map(_.typ))
      cfg.appendBlock(innerHeader)
      val innerBodyEntry =
        freshBlock(Seq(IndexType(), IndexType(), init.typ) ++ prefixResultTypes ++ captures.map(_.typ))
      cfg.appendBlock(innerBodyEntry)
      val outerLatch = freshBlock(Seq(IndexType(), init.typ) ++ captures.map(_.typ))
      cfg.appendBlock(outerLatch)
      val exit = freshBlock(Seq(init.typ))
      cfg.appendBlock(exit)

      // Refined lowering extends the baseline CFG skeleton by threading captured
      // layout-related values through block arguments.
      cfg.emitBr(current, Seq(outerLb, init) ++ captures.map(remap), outerHeader)

      val outerIv = outerHeader.arguments.head
      val outerAcc = outerHeader.arguments(1)
      val outerHeaderCaptures = outerHeader.arguments.drop(2)
      val outerHeaderMap = captures.zip(outerHeaderCaptures).toMap
      val outerUb = lowerBoundWith(
        op.upperBoundOperands,
        op.upperBoundMap,
        v => outerHeaderMap.getOrElse(v, remap(v)),
        outerHeader,
      ).get
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
      prefixOps.foreach(op => lowerSimpleOp(op, Seq(outerBody.arguments.head, outerBody.arguments(1)) ++ captures))
      val prefixValues = prefixOps.flatMap(_.results.map(r => remap(r)))
      val innerLb = lowerBoundWith(
        inner.lowerBoundOperands,
        inner.lowerBoundMap,
        remap,
        outerBodyEntry,
      ).get
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
      val innerHeaderMap =
        captures.zip(innerCaptures).toMap ++
          prefixOps.flatMap(_.results).zip(innerPrefixExtras) ++
          Seq(outerBody.arguments.head -> innerOuterIv, outerBody.arguments(1) -> innerAcc)
      val innerUb = lowerBoundWith(
        inner.upperBoundOperands,
        inner.upperBoundMap,
        v => innerHeaderMap.getOrElse(v, remap(v)),
        innerHeader,
      ).get
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
      val outerLatchArgs = current.arguments
      val outerStep = cfg.emitIndexConstant(current, op.step.value.value)
      val nextOuter = cfg.emitAdd(current, outerLatchArgs.head, outerStep)
      cfg.emitBr(current, Seq(nextOuter, outerLatchArgs(1)) ++ outerLatchArgs.drop(2), outerHeader)

      current = exit
      exit.arguments.head
    }

  private def lowerFor(op: d_affine.For): Option[Value[Attribute]] =
    if hasNestedLoopShape(op) then lowerNestedFor(op)
    else if op.inits.size != 1 || op.res.size != 1 || op.body.blocks.size != 1 then None
    else
      lowerBound(op.lowerBoundOperands, op.lowerBoundMap).map { lb =>
        val init = remap(op.inits.head)
        val bodyBlock = op.body.blocks.head
        val captures =
          (
            op.lowerBoundOperands.map(_.asInstanceOf[Value[Attribute]]) ++
              op.upperBoundOperands.map(_.asInstanceOf[Value[Attribute]]) ++
              collectExternalOperands(bodyBlock)
          ).distinct.filter(needsCapture)
        val header = freshBlock(Seq(IndexType(), init.typ) ++ captures.map(_.typ))
        cfg.appendBlock(header)
        val body = freshBlock(Seq(IndexType(), init.typ) ++ captures.map(_.typ))
        cfg.appendBlock(body)
        val exit = freshBlock(Seq(init.typ))
        cfg.appendBlock(exit)
        cfg.emitBr(current, Seq(lb, init) ++ captures.map(remap), header)
        val iv = header.arguments.head
        val acc = header.arguments(1)
        val headerCaptures = header.arguments.drop(2)
        val headerMap = captures.zip(headerCaptures).toMap
        val ub = lowerBoundWith(
          op.upperBoundOperands,
          op.upperBoundMap,
          v => headerMap.getOrElse(v, remap(v)),
          header,
        ).get
        val cmp = cfg.emitICmpSlt(header, iv, ub)
        cfg.emitCondBr(header, cmp, Seq(iv, acc) ++ headerCaptures, Seq(acc), body, exit)
        current = body
        val saved = mutable.Map.from(state.valueMap)
        state.valueMap.addAll(
          Seq(bodyBlock.arguments.head -> body.arguments.head, bodyBlock.arguments(1) -> body.arguments(1)) ++
            captures.zip(body.arguments.drop(2))
        )
        var yielded: Option[Value[Attribute]] = None
        bodyBlock.operations.foreach {
          case y: d_affine.Yield => yielded = Some(remap(y.args.head))
          case other             => lowerSimpleOp(other, Seq(bodyBlock.arguments.head, bodyBlock.arguments(1)) ++ captures)
        }
        state.valueMap.clear(); state.valueMap.addAll(saved)
        yielded.foreach { y =>
          val step = cfg.emitIndexConstant(current, op.step.value.value)
          val nextIv = cfg.emitAdd(current, remap(bodyBlock.arguments.head), step)
          cfg.emitBr(current, Seq(nextIv, y) ++ captures.map(remap), header)
        }
        current = exit
        exit.arguments.head
      }

  private def lowerVoidFor(
      op: d_affine.For,
      continuationCaptures: Seq[Value[Attribute]],
  ): Boolean =
    if op.inits.nonEmpty || op.res.nonEmpty || op.body.blocks.size != 1 then false
    else
      val bodyBlock = op.body.blocks.head
      val controlOperands =
        (op.lowerBoundOperands ++ op.upperBoundOperands).map(_.asInstanceOf[Value[Attribute]])
      val captures =
        (controlOperands ++ collectExternalOperands(bodyBlock) ++ continuationCaptures).distinct.filter(needsCapture)
      val remappedCaptures = captures.map(remap)

      val header = freshBlock(Seq(IndexType()) ++ remappedCaptures.map(_.typ))
      cfg.appendBlock(header)
      val body = freshBlock(Seq(IndexType()) ++ remappedCaptures.map(_.typ))
      cfg.appendBlock(body)
      val exit = freshBlock(remappedCaptures.map(_.typ))
      cfg.appendBlock(exit)

      val preheaderMap = captures.zip(remappedCaptures).toMap
      val lb = lowerBoundWith(
        op.lowerBoundOperands,
        op.lowerBoundMap,
        v => preheaderMap.getOrElse(v, remap(v)),
        current,
      )
      lb match
        case None => false
        case Some(lbVal) =>
          cfg.emitBr(current, Seq(lbVal) ++ remappedCaptures, header)
          val iv = header.arguments.head
          val headerCaptures = header.arguments.drop(1).toSeq
          val headerMap = captures.zip(headerCaptures).toMap
          val ub = lowerBoundWith(
            op.upperBoundOperands,
            op.upperBoundMap,
            v => headerMap.getOrElse(v, remap(v)),
            header,
          )
          ub match
            case None => false
            case Some(ubVal) =>
              val cmp = cfg.emitICmpSlt(header, iv, ubVal)
              cfg.emitCondBr(header, cmp, Seq(iv) ++ headerCaptures, headerCaptures, body, exit)
              current = body
              val saved = mutable.Map.from(state.valueMap)
              state.valueMap.addAll(
                Seq(bodyBlock.arguments.head -> body.arguments.head) ++ captures.zip(body.arguments.drop(1))
              )
              bodyBlock.operations.toSeq.foreach {
                case _: d_affine.Yield =>
                case nested: d_affine.For =>
                  lowerLoop(
                    nested,
                    Seq(bodyBlock.arguments.head) ++ captures ++ continuationCaptures,
                  )
                case other =>
                  lowerSimpleOp(other, Seq(bodyBlock.arguments.head) ++ captures ++ continuationCaptures)
              }
              val liveArgs = current.arguments
              val liveIv = liveArgs.head
              val liveCaptures = liveArgs.drop(1)
              val step = cfg.emitIndexConstant(current, op.step.value.value)
              val nextIv = cfg.emitAdd(current, liveIv, step)
              cfg.emitBr(current, Seq(nextIv) ++ liveCaptures, header)
              current = exit
              state.valueMap.clear(); state.valueMap.addAll(saved)
              state.valueMap.addAll(captures.zip(exit.arguments))
              true

  private def lowerLoop(
      op: d_affine.For,
      continuationCaptures: Seq[Value[Attribute]] = Seq.empty,
  ): Unit =
    if op.inits.isEmpty && op.res.isEmpty then
      lowerVoidFor(op, continuationCaptures)
    else
      lowerFor(op).foreach(v => state.valueMap(op.res.head) = v)

  def lower(): func.Func =
    funcOp.body.blocks.head.operations.foreach {
      case loop: d_affine.For =>
        lowerLoop(loop)
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

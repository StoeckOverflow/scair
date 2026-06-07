package scair.passes.lower_refined_control_flow_to_llvm

import scair.MLContext
import scair.dialects.affine
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.dialects.func
import scair.dialects.scf
import scair.ir.*
import scair.passes.control_flow_helpers.*
import scair.passes.lowering_helpers.FunctionLoweringState
import scair.passes.NatProvenance
import scair.transformations.*

import scala.collection.mutable

// This builder reconstructs refined structured control flow as LLVM CFG while
// using block arguments only for true loop-carried state. Dominating refined
// values are referenced directly through SSA, mirroring MLIR's CFG style.
private final class Builder(val funcOp: func.Func):
  private val state = FunctionLoweringState(funcOp)
  private val blocks = mutable.ArrayBuffer.empty[Block]
  private val cfg = LoopCFGBuilder(blocks)

  private var current: Block =
    Block.cloneArgumentTypes(funcOp.body.blocks.head.arguments, Seq.empty)(using
      state.valueMap
    )

  blocks += current
  state.blockMap(funcOp.body.blocks.head) = current

  private def remap(v: Value[Attribute]): Value[Attribute] =
    state.remap(v)

  private def unsupportedLoop(reason: String): Nothing =
    throw new Exception(
      s"lower-refined-control-flow-to-llvm cannot lower d_affine.for: $reason. " +
        "Supported affine maps are constants, dims/symbols, add/sub, and constant multiplication; " +
        "run d-affine-to-affine-compatible first when the IR should enter the stock affine pipeline."
    )

  private def lowerBound(
      operands: Seq[Value[Attribute]],
      map: AffineMapAttr,
  ): Option[Value[Attribute]] =
    cfg.materializeAffineMap(current, operands.map(remap), map)

  private def deepCopyOp(op: Operation): Operation =
    state.deepCopyOp(op)

  private def freshBlock(argumentTypes: Seq[Attribute]): Block =
    Block.cloneAndRemapArgumentTypes(argumentTypes, Seq.empty)(using
      state.valueMap
    )

  private def lowerBoundWith(
      operands: Seq[Value[Attribute]],
      map: AffineMapAttr,
      remapper: Value[Attribute] => Value[Attribute],
      block: Block,
  ): Option[Value[Attribute]] =
    cfg.materializeAffineMap(block, operands.map(remapper), map)

  private def lowerAffineApplyLike(
      operands: Seq[Value[Attribute]],
      map: AffineMapAttr,
      result: Value[Attribute],
  ): Unit =
    val lowered = cfg.materializeAffineMap(current, operands.map(remap), map)
      .getOrElse(
        unsupportedLoop(
          explainUnsupportedAffineMap(map)
            .getOrElse("unsupported d_affine.apply/min affine map")
        )
      )
    state.valueMap(result) = lowered

  private def lowerStepWith(
      op: d_affine.For,
      remapper: Value[Attribute] => Value[Attribute],
      block: Block,
      like: Value[Attribute],
  ): Value[Attribute] =
    op.stepOperands.headOption match
      case Some(dynamicStep) => remapper(dynamicStep)
      case None => cfg.emitIndexConstantLike(block, op.step.value.value, like)

  private def lowerSimpleOp(
      op: Operation
  ): Unit =
    op match
      case apply: d_affine.Apply =>
        lowerAffineApplyLike(
          apply.dimOperands ++ apply.symbolOperands,
          apply.map,
          apply.res,
        )
      case min: d_affine.Min =>
        lowerAffineApplyLike(
          min.dimOperands ++ min.symbolOperands,
          min.map,
          min.res,
        )
      case nested: d_affine.For =>
        lowerLoop(nested)
      case nested: affine.For =>
        lowerAffineFor(nested)
      case ifOp: d_affine.If =>
        lowerDAffineIf(ifOp)
      case ifOp: affine.If =>
        lowerAffineIf(ifOp)
      case ifOp: scf.IfOp =>
        lowerIf(ifOp)
      case other =>
        val copied = deepCopyOp(other)
        current.addOp(copied)
        state.valueMap.addAll(op.results.zip(copied.results))

  private def lowerIfRegions(
      condition: Value[Attribute],
      thenBlockSrc: Block,
      elseBlockSrc: Block,
      results: Seq[Value[Attribute]],
      yieldValues: Operation => Option[Seq[Value[Attribute]]],
  ): Unit =
    val thenBlock = freshBlock(Seq.empty)
    val elseBlock = freshBlock(Seq.empty)
    val merge = freshBlock(results.map(_.typ))
    cfg.appendBlock(thenBlock)
    cfg.appendBlock(elseBlock)
    cfg.appendBlock(merge)
    cfg.emitCondBr(
      current,
      condition,
      Seq.empty,
      Seq.empty,
      thenBlock,
      elseBlock,
    )

    def lowerRegion(src: Block, dest: Block): Unit =
      current = dest
      var yielded: Seq[Value[Attribute]] = Seq.empty
      src.operations.foreach { op =>
        yieldValues(op) match
          case Some(values) => yielded = values.map(remap)
          case None         => lowerSimpleOp(op)
      }
      cfg.emitBr(current, yielded, merge)

    lowerRegion(thenBlockSrc, thenBlock)
    lowerRegion(elseBlockSrc, elseBlock)
    current = merge
    state.valueMap.addAll(results.zip(merge.arguments))

  private def lowerIf(op: scf.IfOp): Unit =
    if op.thenRegion.blocks.size != 1 || op.elseRegion.blocks.size != 1 then
      throw new Exception(
        "lower-refined-control-flow-to-llvm only supports single-block scf.if"
      )
    lowerIfRegions(
      remap(op.condition),
      op.thenRegion.blocks.head,
      op.elseRegion.blocks.head,
      op.results,
      {
        case y: scf.YieldOp => Some(y.resultss)
        case _              => None
      },
    )

  private def lowerDAffineIf(op: d_affine.If): Unit =
    if op.thenRegion.blocks.size != 1 || op.elseRegion.blocks.size != 1 then
      throw new Exception(
        "lower-refined-control-flow-to-llvm only supports single-block d_affine.if"
      )
    val condition = cfg
      .materializeAffineSet(current, op.args.map(remap), op.condition)
      .getOrElse(unsupportedLoop("unsupported d_affine.if affine set"))
    lowerIfRegions(
      condition,
      op.thenRegion.blocks.head,
      op.elseRegion.blocks.head,
      op.results,
      {
        case y: d_affine.Yield => Some(y.args)
        case _                 => None
      },
    )

  private def lowerAffineIf(op: affine.If): Unit =
    if op.thenRegion.blocks.size != 1 || op.elseRegion.blocks.size != 1 then
      throw new Exception(
        "lower-refined-control-flow-to-llvm only supports single-block affine.if"
      )
    val condition = cfg
      .materializeAffineSet(current, op.args.map(remap), op.condition)
      .getOrElse(unsupportedLoop("unsupported affine.if affine set"))
    lowerIfRegions(
      condition,
      op.thenRegion.blocks.head,
      op.elseRegion.blocks.head,
      op.results,
      {
        case y: affine.Yield => Some(y.arguments)
        case _               => None
      },
    )

  private def hasNestedLoopShape(op: d_affine.For): Boolean =
    if op.inits.size != 1 || op.res.size != 1 || op.body.blocks.size != 1 then
      false
    else
      op.body.blocks.head.operations.toSeq match
        case ops if ops.size >= 2 =>
          ops.last.isInstanceOf[d_affine.Yield] && ops(ops.size - 2)
            .isInstanceOf[d_affine.For]
        case _ => false

  private def lowerNestedFor(op: d_affine.For): Option[Value[Attribute]] =
    val outerBody = op.body.blocks.head
    val outerOps = outerBody.operations.toSeq
    val prefixOps = outerOps.dropRight(2)
    val inner = outerOps(outerOps.size - 2).asInstanceOf[d_affine.For]
    val innerBody = inner.body.blocks.head
    lowerBound(op.lowerBoundOperands, op.lowerBoundMap).map { outerLb =>
      val init = remap(op.inits.head)
      val outerHeader = freshBlock(Seq(IndexType(), init.typ))
      cfg.appendBlock(outerHeader)
      val outerBodyEntry = freshBlock(Seq.empty)
      cfg.appendBlock(outerBodyEntry)
      val prefixResultTypes = prefixOps.flatMap(_.results.map(_.typ))
      val innerHeader =
        freshBlock(Seq(IndexType(), IndexType(), init.typ) ++ prefixResultTypes)
      cfg.appendBlock(innerHeader)
      val innerBodyEntry =
        freshBlock(Seq.empty)
      cfg.appendBlock(innerBodyEntry)
      val outerLatch = freshBlock(Seq.empty)
      cfg.appendBlock(outerLatch)
      val exit = freshBlock(Seq.empty)
      cfg.appendBlock(exit)

      cfg.emitBr(current, Seq(outerLb, init), outerHeader)

      val outerIv = outerHeader.arguments.head
      val outerAcc = outerHeader.arguments(1)
      val outerUb = lowerBoundWith(
        op.upperBoundOperands,
        op.upperBoundMap,
        remap,
        outerHeader,
      ).getOrElse(unsupportedLoop("unsupported outer upper bound"))
      val outerCmp = cfg.emitICmpSlt(outerHeader, outerIv, outerUb)
      cfg.emitCondBr(
        outerHeader,
        outerCmp,
        Seq.empty,
        Seq.empty,
        outerBodyEntry,
        exit,
      )

      current = outerBodyEntry
      state.valueMap
        .addAll(
          Seq(
            outerBody.arguments.head -> outerIv,
            outerBody.arguments(1) -> outerAcc,
          )
        )
      prefixOps.foreach(op => lowerSimpleOp(op))
      val prefixValues = prefixOps.flatMap(_.results.map(r => remap(r)))
      val innerLb = lowerBoundWith(
        inner.lowerBoundOperands,
        inner.lowerBoundMap,
        remap,
        outerBodyEntry,
      ).getOrElse(unsupportedLoop("unsupported inner lower bound"))
      cfg.emitBr(
        outerBodyEntry,
        Seq(outerIv, innerLb, outerAcc) ++ prefixValues,
        innerHeader,
      )

      val innerOuterIv = innerHeader.arguments.head
      val innerIv = innerHeader.arguments(1)
      val innerAcc = innerHeader.arguments(2)
      val innerPrefixExtras = innerHeader.arguments.drop(3)
        .take(prefixResultTypes.size)
      val innerHeaderMap =
        prefixOps.flatMap(_.results).zip(innerPrefixExtras) ++ Seq(
          outerBody.arguments.head -> innerOuterIv,
          outerBody.arguments(1) -> innerAcc,
        )
      val innerUb = lowerBoundWith(
        inner.upperBoundOperands,
        inner.upperBoundMap,
        v => innerHeaderMap.toMap.getOrElse(v, remap(v)),
        innerHeader,
      ).getOrElse(unsupportedLoop("unsupported inner upper bound"))
      val innerCmp = cfg.emitICmpSlt(innerHeader, innerIv, innerUb)
      cfg.emitCondBr(
        innerHeader,
        innerCmp,
        Seq.empty,
        Seq.empty,
        innerBodyEntry,
        outerLatch,
      )

      current = innerBodyEntry
      state.valueMap
        .addAll(
          Seq(
            op.body.blocks.head.arguments.head -> innerOuterIv,
            op.body.blocks.head.arguments(1) -> outerAcc,
            innerBody.arguments.head -> innerIv,
            innerBody.arguments(1) -> innerAcc,
          ) ++ prefixOps.flatMap(_.results).zip(innerPrefixExtras)
        )
      var yielded: Option[Value[Attribute]] = None
      innerBody.operations.foreach {
        case y: d_affine.Yield => yielded = Some(remap(y.args.head))
        case other             => lowerSimpleOp(other)
      }
      yielded.foreach { y =>
        val step = lowerStepWith(
          inner,
          v => innerHeaderMap.toMap.getOrElse(v, remap(v)),
          current,
          innerIv,
        )
        val nextIv = cfg.emitAdd(current, innerIv, step)
        cfg.emitBr(
          current,
          Seq(innerOuterIv, nextIv, y) ++ innerPrefixExtras,
          innerHeader,
        )
      }

      current = outerLatch
      val outerStep = lowerStepWith(op, remap, current, innerOuterIv)
      val nextOuter = cfg.emitAdd(current, innerOuterIv, outerStep)
      cfg.emitBr(current, Seq(nextOuter, innerAcc), outerHeader)

      current = exit
      outerAcc
    }

  private def lowerFor(op: d_affine.For): Option[Value[Attribute]] =
    if hasNestedLoopShape(op) then lowerNestedFor(op)
    else if op.inits.size != 1 || op.res.size != 1 || op.body.blocks.size != 1
    then None
    else
      lowerBound(op.lowerBoundOperands, op.lowerBoundMap).map { lb =>
        val init = remap(op.inits.head)
        val bodyBlock = op.body.blocks.head
        val header = freshBlock(Seq(IndexType(), init.typ))
        cfg.appendBlock(header)
        val body = freshBlock(Seq.empty)
        cfg.appendBlock(body)
        val exit = freshBlock(Seq.empty)
        cfg.appendBlock(exit)
        cfg.emitBr(current, Seq(lb, init), header)
        val iv = header.arguments.head
        val acc = header.arguments(1)
        val ub = lowerBoundWith(
          op.upperBoundOperands,
          op.upperBoundMap,
          remap,
          header,
        ).getOrElse(unsupportedLoop("unsupported loop upper bound"))
        val cmp = cfg.emitICmpSlt(header, iv, ub)
        cfg.emitCondBr(header, cmp, Seq.empty, Seq.empty, body, exit)
        current = body
        state.valueMap
          .addAll(
            Seq(bodyBlock.arguments.head -> iv, bodyBlock.arguments(1) -> acc)
          )
        var yielded: Option[Value[Attribute]] = None
        bodyBlock.operations.foreach {
          case y: d_affine.Yield => yielded = Some(remap(y.args.head))
          case other             => lowerSimpleOp(other)
        }
        yielded.foreach { y =>
          val step = lowerStepWith(op, remap, current, iv)
          val nextIv = cfg.emitAdd(current, iv, step)
          cfg.emitBr(current, Seq(nextIv, y), header)
        }
        current = exit
        acc
      }

  private def lowerMultiResultFor(
      op: d_affine.For
  ): Option[Seq[Value[Attribute]]] =
    if op.body.blocks.size != 1 || op.inits.isEmpty ||
      op.inits.size != op.res.size
    then None
    else
      lowerBound(op.lowerBoundOperands, op.lowerBoundMap).map { lb =>
        val initVals = op.inits.map(remap)
        val bodyBlock = op.body.blocks.head
        val header = freshBlock(Seq(IndexType()) ++ initVals.map(_.typ))
        cfg.appendBlock(header)
        val body = freshBlock(Seq.empty)
        cfg.appendBlock(body)
        val exit = freshBlock(op.res.map(_.typ))
        cfg.appendBlock(exit)
        cfg.emitBr(current, Seq(lb) ++ initVals, header)
        val iv = header.arguments.head
        val carried = header.arguments.tail.toSeq
        val ub = lowerBoundWith(
          op.upperBoundOperands,
          op.upperBoundMap,
          remap,
          header,
        ).getOrElse(unsupportedLoop("unsupported loop upper bound"))
        val cmp = cfg.emitICmpSlt(header, iv, ub)
        cfg.emitCondBr(header, cmp, Seq.empty, carried, body, exit)
        current = body
        state.valueMap.addAll(bodyBlock.arguments.zip(Seq(iv) ++ carried))
        var yielded: Seq[Value[Attribute]] = Seq.empty
        bodyBlock.operations.foreach {
          case y: d_affine.Yield => yielded = y.args.map(remap)
          case other             => lowerSimpleOp(other)
        }
        val step = lowerStepWith(op, remap, current, iv)
        val nextIv = cfg.emitAdd(current, iv, step)
        cfg.emitBr(current, Seq(nextIv) ++ yielded, header)
        current = exit
        exit.arguments.toSeq
      }

  private def lowerVoidFor(
      op: d_affine.For
  ): Boolean =
    if op.inits.nonEmpty || op.res.nonEmpty || op.body.blocks.size != 1 then
      false
    else
      val bodyBlock = op.body.blocks.head
      val header = freshBlock(Seq(IndexType()))
      cfg.appendBlock(header)
      val body = freshBlock(Seq.empty)
      cfg.appendBlock(body)
      val exit = freshBlock(Seq.empty)
      cfg.appendBlock(exit)

      val lb = lowerBoundWith(
        op.lowerBoundOperands,
        op.lowerBoundMap,
        remap,
        current,
      )
      lb match
        case None        => false
        case Some(lbVal) =>
          cfg.emitBr(current, Seq(lbVal), header)
          val iv = header.arguments.head
          val ub = lowerBoundWith(
            op.upperBoundOperands,
            op.upperBoundMap,
            remap,
            header,
          )
          ub match
            case None        => false
            case Some(ubVal) =>
              val cmp = cfg.emitICmpSlt(header, iv, ubVal)
              cfg.emitCondBr(header, cmp, Seq.empty, Seq.empty, body, exit)
              current = body
              state.valueMap
                .addAll(
                  Seq(bodyBlock.arguments.head -> iv)
                )
              bodyBlock.operations.toSeq.foreach {
                case _: d_affine.Yield    =>
                case nested: d_affine.For =>
                  lowerLoop(nested)
                case other =>
                  lowerSimpleOp(other)
              }
              val step = lowerStepWith(op, remap, current, iv)
              val nextIv = cfg.emitAdd(current, iv, step)
              cfg.emitBr(current, Seq(nextIv), header)
              current = exit
              true

  private def lowerAffineFor(op: affine.For): Unit =
    if op.inits.isEmpty && op.res.isEmpty then
      if !lowerAffineVoidFor(op) then
        unsupportedLoop("unsupported affine.for void loop shape or bound")
    else if op.body.blocks.size != 1 || op.inits.size != op.res.size then
      unsupportedLoop("unsupported affine.for iter_args/result contract")
    else
      lowerAffineResultFor(op) match
        case Some(vals) => state.valueMap.addAll(op.res.zip(vals))
        case None       =>
          unsupportedLoop("unsupported affine.for result loop shape or bound")

  private def lowerAffineVoidFor(op: affine.For): Boolean =
    if op.inits.nonEmpty || op.res.nonEmpty || op.body.blocks.size != 1 then
      false
    else
      val bodyBlock = op.body.blocks.head
      val header = freshBlock(Seq(IndexType()))
      cfg.appendBlock(header)
      val body = freshBlock(Seq.empty)
      cfg.appendBlock(body)
      val exit = freshBlock(Seq.empty)
      cfg.appendBlock(exit)

      lowerBoundWith(
        op.lowerBoundOperands,
        op.lowerBoundMap,
        remap,
        current,
      ) match
        case None        => false
        case Some(lbVal) =>
          cfg.emitBr(current, Seq(lbVal), header)
          val iv = header.arguments.head
          lowerBoundWith(
            op.upperBoundOperands,
            op.upperBoundMap,
            remap,
            header,
          ) match
            case None        => false
            case Some(ubVal) =>
              val cmp = cfg.emitICmpSlt(header, iv, ubVal)
              cfg.emitCondBr(header, cmp, Seq.empty, Seq.empty, body, exit)
              current = body
              state.valueMap.addAll(Seq(bodyBlock.arguments.head -> iv))
              bodyBlock.operations.toSeq.foreach {
                case _: affine.Yield =>
                case other           => lowerSimpleOp(other)
              }
              val step =
                cfg.emitIndexConstantLike(current, op.step.value.value, iv)
              val nextIv = cfg.emitAdd(current, iv, step)
              cfg.emitBr(current, Seq(nextIv), header)
              current = exit
              true

  private def lowerAffineResultFor(
      op: affine.For
  ): Option[Seq[Value[Attribute]]] =
    if op.body.blocks.size != 1 || op.inits.isEmpty ||
      op.inits.size != op.res.size
    then None
    else
      lowerBoundWith(op.lowerBoundOperands, op.lowerBoundMap, remap, current)
        .map { lb =>
          val initVals = op.inits.map(remap)
          val bodyBlock = op.body.blocks.head
          val header = freshBlock(Seq(IndexType()) ++ initVals.map(_.typ))
          cfg.appendBlock(header)
          val body = freshBlock(Seq.empty)
          cfg.appendBlock(body)
          val exit = freshBlock(op.res.map(_.typ))
          cfg.appendBlock(exit)
          cfg.emitBr(current, Seq(lb) ++ initVals, header)
          val iv = header.arguments.head
          val carried = header.arguments.tail.toSeq
          val ub = lowerBoundWith(
            op.upperBoundOperands,
            op.upperBoundMap,
            remap,
            header,
          ).getOrElse(unsupportedLoop("unsupported affine.for upper bound"))
          val cmp = cfg.emitICmpSlt(header, iv, ub)
          cfg.emitCondBr(header, cmp, Seq.empty, carried, body, exit)
          current = body
          state.valueMap.addAll(bodyBlock.arguments.zip(Seq(iv) ++ carried))
          var yielded: Seq[Value[Attribute]] = Seq.empty
          bodyBlock.operations.foreach {
            case y: affine.Yield => yielded = y.arguments.map(remap)
            case other           => lowerSimpleOp(other)
          }
          val step =
            cfg.emitIndexConstantLike(current, op.step.value.value, iv)
          val nextIv = cfg.emitAdd(current, iv, step)
          cfg.emitBr(current, Seq(nextIv) ++ yielded, header)
          current = exit
          exit.arguments.toSeq
        }

  private def lowerLoop(
      op: d_affine.For
  ): Unit =
    op.stepOperands.headOption.foreach { step =>
      if !NatProvenance.isPositive(step) then
        unsupportedLoop("dynamic step is not proven strictly positive")
    }
    if op.inits.isEmpty && op.res.isEmpty then
      if !lowerVoidFor(op) then
        unsupportedLoop("unsupported void loop shape or bound")
    else if op.res.size > 1 then
      lowerMultiResultFor(op) match
        case Some(vals) => state.valueMap.addAll(op.res.zip(vals))
        case None       =>
          unsupportedLoop("unsupported multi-result loop shape or lower bound")
    else
      lowerFor(op) match
        case Some(v) => state.valueMap(op.res.head) = v
        case None    =>
          unsupportedLoop("unsupported single-result loop shape or lower bound")

  def lower(): func.Func =
    funcOp.body.blocks.head.operations.foreach {
      case loop: d_affine.For =>
        lowerLoop(loop)
      case loop: affine.For =>
        lowerAffineFor(loop)
      case ifOp: d_affine.If =>
        lowerDAffineIf(ifOp)
      case ifOp: affine.If =>
        lowerAffineIf(ifOp)
      case ifOp: scf.IfOp =>
        lowerIf(ifOp)
      case other =>
        lowerSimpleOp(other)
    }
    val loweredFunctionType =
      AttributeWalker.cloneValueAttributes(funcOp.function_type)
        .asInstanceOf[FunctionType]
    AttributeWalker
      .remapTypeUsesInPlace(loweredFunctionType)(using
        state.valueMap
      )
    val lowered = func.Func(
      funcOp.sym_name,
      loweredFunctionType,
      funcOp.sym_visibility,
      Region(blocks.toSeq),
    )
    lowered.attributes
      .addAll(
        funcOp.attributes.view.mapValues(AttributeWalker.cloneValueAttributes)
      )
    lowered.attributes.values.foreach(attr =>
      AttributeWalker.remapTypeUsesInPlace(attr)(using state.valueMap)
    )
    lowered

private def lowerFunc(op: func.Func): Option[func.Func] =
  if op.body.blocks.size != 1 then None else Some(Builder(op).lower())

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists {
        case _: d_affine.For | _: d_affine.Apply | _: d_affine.Min |
            _: d_affine.If | _: affine.For | _: affine.If | _: scf.IfOp =>
          true
        case _ => false
      }) =>
    lowerFunc(op).getOrElse(
      throw new Exception(
        "lower-refined-control-flow-to-llvm only supports single-block func.func bodies"
      )
    )
}

// Lowers refined affine control flow to explicit LLVM CFG.
// Example: `d_affine.for` / `d_affine.yield`
//   -> `llvm.br`, `llvm.cond_br`, and block arguments carrying loop state plus
//      captured refined values.
final class LowerRefinedControlFlowToLLVM(ctx: MLContext)
    extends WalkerPass(ctx):
  override val name: String = "lower-refined-control-flow-to-llvm"

  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))

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

  private def lowerSimpleOp(
      op: Operation,
  ): Unit =
    op match
      case nested: For =>
        lowerLoop(nested)
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
    lowerBound(op.lowerBoundOperands, op.lowerBoundMap).map { outerLb =>
      val init = remap(op.inits.head)
      val outerHeader = Block(Seq(llvmIndexType, init.typ), Seq.empty)
      cfg.appendBlock(outerHeader)
      val outerBodyEntry = Block(Seq.empty, Seq.empty)
      cfg.appendBlock(outerBodyEntry)
      val innerHeader = Block(Seq(llvmIndexType, init.typ), Seq.empty)
      cfg.appendBlock(innerHeader)
      val innerBodyEntry = Block(Seq.empty, Seq.empty)
      cfg.appendBlock(innerBodyEntry)
      val outerLatch = Block(Seq.empty, Seq.empty)
      cfg.appendBlock(outerLatch)
      val exit = Block(Seq.empty, Seq.empty)
      cfg.appendBlock(exit)

      // The nested-loop lowering uses block arguments only for true loop-carried
      // values, relying on SSA dominance for invariant descriptor/layout state.
      cfg.emitBr(current, Seq(outerLb, init), outerHeader)

      val outerIv = outerHeader.arguments.head
      val outerAcc = outerHeader.arguments(1)
      val outerUb = lowerBoundWith(
        op.upperBoundOperands,
        op.upperBoundMap,
        remap,
        outerHeader,
      ).get
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
      state.valueMap.addAll(
        Seq(outerBody.arguments.head -> outerIv, outerBody.arguments(1) -> outerAcc)
      )
      prefixOps.foreach(op => lowerSimpleOp(op))
      val innerLb = lowerBoundWith(
        inner.lowerBoundOperands,
        inner.lowerBoundMap,
        remap,
        outerBodyEntry,
      ).get
      cfg.emitBr(
        outerBodyEntry,
        Seq(innerLb, outerAcc),
        innerHeader,
      )

      val innerIv = innerHeader.arguments.head
      val innerAcc = innerHeader.arguments(1)
      val innerUb = lowerBoundWith(
        inner.upperBoundOperands,
        inner.upperBoundMap,
        remap,
        innerHeader,
      ).get
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
      state.valueMap.addAll(
        Seq(
          outerBody.arguments.head -> outerIv,
          outerBody.arguments(1) -> outerAcc,
          innerBody.arguments.head -> innerIv,
          innerBody.arguments(1) -> innerAcc,
        )
      )
      var yielded: Option[Value[Attribute]] = None
      innerBody.operations.foreach {
        case y: Yield => yielded = Some(remap(y.arguments.head))
        case other    => lowerSimpleOp(other)
      }
      yielded.foreach { y =>
        val step = cfg.emitIndexConstant(current, op.step.value.value)
        val nextIv = cfg.emitAdd(current, innerIv, step)
        cfg.emitBr(
          current,
          Seq(nextIv, y),
          innerHeader,
        )
      }

      current = outerLatch
      val outerStep = cfg.emitIndexConstant(current, op.step.value.value)
      val nextOuter = cfg.emitAdd(current, outerIv, outerStep)
      cfg.emitBr(current, Seq(nextOuter, innerAcc), outerHeader)

      current = exit
      outerAcc
    }

  private def lowerFor(op: For): Option[Value[Attribute]] =
    if hasNestedLoopShape(op) then lowerNestedFor(op)
    else if op.inits.size != 1 || op.res.size != 1 || op.body.blocks.size != 1 then None
    else
      lowerBound(op.lowerBoundOperands, op.lowerBoundMap).map { lb =>
        val init = remap(op.inits.head)
        val bodyBlock = op.body.blocks.head
        val header = Block(Seq(llvmIndexType, init.typ), Seq.empty)
        cfg.appendBlock(header)
        val body = Block(Seq.empty, Seq.empty)
        cfg.appendBlock(body)
        val exit = Block(Seq.empty, Seq.empty)
        cfg.appendBlock(exit)
        cfg.emitBr(current, Seq(lb, init), header)
        val iv = header.arguments.head
        val acc = header.arguments(1)
        val ub = lowerBoundWith(
          op.upperBoundOperands,
          op.upperBoundMap,
          remap,
          header,
        ).get
        val cmp = cfg.emitICmpSlt(header, iv, ub)
        cfg.emitCondBr(header, cmp, Seq.empty, Seq.empty, body, exit)
        current = body
        state.valueMap.addAll(
          Seq(bodyBlock.arguments.head -> iv, bodyBlock.arguments(1) -> acc)
        )
        var yielded: Option[Value[Attribute]] = None
        bodyBlock.operations.foreach {
          case y: Yield => yielded = Some(remap(y.arguments.head))
          case other    => lowerSimpleOp(other)
        }
        yielded.foreach { y =>
          val step = cfg.emitIndexConstant(current, op.step.value.value)
          val nextIv = cfg.emitAdd(current, iv, step)
          cfg.emitBr(current, Seq(nextIv, y), header)
        }
        current = exit
        acc
      }

  private def lowerVoidFor(
      op: For,
  ): Boolean =
    if op.inits.nonEmpty || op.res.nonEmpty || op.body.blocks.size != 1 then false
    else
      val bodyBlock = op.body.blocks.head
      val header = Block(Seq(llvmIndexType), Seq.empty)
      cfg.appendBlock(header)
      val body = Block(Seq.empty, Seq.empty)
      cfg.appendBlock(body)
      val exit = Block(Seq.empty, Seq.empty)
      cfg.appendBlock(exit)

      val lb = lowerBoundWith(
        op.lowerBoundOperands,
        op.lowerBoundMap,
        remap,
        current,
      )
      lb match
        case None => false
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
            case None => false
            case Some(ubVal) =>
              val cmp = cfg.emitICmpSlt(header, iv, ubVal)
              cfg.emitCondBr(header, cmp, Seq.empty, Seq.empty, body, exit)
              current = body
              state.valueMap.addAll(
                Seq(bodyBlock.arguments.head -> iv)
              )
              bodyBlock.operations.toSeq.foreach {
                case _: Yield =>
                case nested: For =>
                  lowerLoop(nested)
                case other =>
                  lowerSimpleOp(other)
              }
              val step = cfg.emitIndexConstant(current, op.step.value.value)
              val nextIv = cfg.emitAdd(current, iv, step)
              cfg.emitBr(current, Seq(nextIv), header)
              current = exit
              true

  private def lowerLoop(
      op: For,
  ): Unit =
    if op.inits.isEmpty && op.res.isEmpty then
      lowerVoidFor(op)
    else
      lowerFor(op).foreach(v => state.valueMap(op.results.head) = v)

  def lower(): func.Func =
    funcOp.body.blocks.head.operations.foreach {
      case loop: For => lowerLoop(loop)
      case other     => lowerSimpleOp(other)
    }
    val lowered = func.Func(funcOp.sym_name, funcOp.function_type, funcOp.sym_visibility, Region(blocks.toSeq))
    lowered.attributes.addAll(funcOp.attributes)
    lowered

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

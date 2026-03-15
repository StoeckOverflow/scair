package scair.passes.lower_baseline_control_flow_to_llvm

import scair.MLContext
import scair.dialects.affine.*
import scair.dialects.builtin.*
import scair.dialects.func
import scair.dialects.llvm
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

import scala.collection.mutable

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def asI1(v: Value[Attribute]): Operand[IntegerType] =
  v.asInstanceOf[Operand[IntegerType]]

private def idxAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), IndexType())

private def overflowNSWNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("nsw"), StringData("nuw")))

private def identityOrConstBound(
    operands: Seq[Value[Attribute]],
    map: AffineMapAttr,
): Option[Either[BigInt, Value[Attribute]]] =
  if map.affineMap.affineExprs.size != 1 then None
  else
    val dims = map.affineMap.dimensions
    map.affineMap.affineExprs.head match
      case AffineConstantExpr(v) => Some(Left(v))
      case AffineDimExpr(name) =>
        val idx = dims.indexOf(name)
        if idx < 0 || idx >= operands.size then None else Some(Right(operands(idx)))
      case _ => None

private final class Builder(val funcOp: func.Func):
  val blocks = mutable.ArrayBuffer.empty[Block]
  val blockMap = mutable.Map.empty[Block, Block]
  val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]
  var current: Block = Block(funcOp.body.blocks.head.arguments.map(_.typ), Seq.empty)
  blocks += current
  blockMap(funcOp.body.blocks.head) = current
  valueMap.addAll(funcOp.body.blocks.head.arguments.zip(current.arguments))

  private def emit(op: Operation): Unit = current.addOp(op)
  private def appendBlock(block: Block): Unit = blocks += block
  private def emitIndexConstant(v: BigInt): Value[Attribute] =
    val c = llvm.Constant(idxAttr(v), Result(IndexType()))
    emit(c)
    c.res
  private def remap(v: Value[Attribute]): Value[Attribute] = valueMap.getOrElse(v, v)
  private def lowerBound(operands: Seq[Value[Attribute]], map: AffineMapAttr): Option[Value[Attribute]] =
    identityOrConstBound(operands.map(remap), map).map {
      case Left(k)  => emitIndexConstant(k)
      case Right(v) => remap(v)
    }
  private def deepCopyOp(op: Operation): Operation = op.deepCopy(using blockMap, valueMap)

  private def lowerSimpleOp(op: Operation): Unit =
    op match
      case nested: For =>
        lowerFor(nested).foreach(v => valueMap(nested.results.head) = v)
      case other =>
        val copied = deepCopyOp(other)
        emit(copied)
        valueMap.addAll(op.results.zip(copied.results))

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
      appendBlock(outerHeader)
      val outerBodyEntry = Block(Seq(IndexType(), init.typ), Seq.empty)
      appendBlock(outerBodyEntry)
      val prefixResultTypes = prefixOps.flatMap(_.results.map(_.typ))
      val innerHeader = Block(Seq(IndexType(), IndexType(), init.typ) ++ prefixResultTypes, Seq.empty)
      appendBlock(innerHeader)
      val innerBodyEntry = Block(Seq(IndexType(), IndexType(), init.typ) ++ prefixResultTypes, Seq.empty)
      appendBlock(innerBodyEntry)
      val outerLatch = Block(Seq(IndexType(), init.typ), Seq.empty)
      appendBlock(outerLatch)
      val exit = Block(Seq(init.typ), Seq.empty)
      appendBlock(exit)

      emit(llvm.Br(Seq(asIndex(outerLb), init.asInstanceOf[Operand[Attribute]]), outerHeader))

      val outerIv = outerHeader.arguments.head
      val outerAcc = outerHeader.arguments(1)
      val outerCmp = llvm.ICmp(asIndex(outerIv), asIndex(outerUb), StringData("slt"), Result(I1))
      outerHeader.addOp(outerCmp)
      outerHeader.addOp(
        llvm.CondBr(
          asI1(outerCmp.res),
          Seq(outerIv.asInstanceOf[Operand[Attribute]], outerAcc.asInstanceOf[Operand[Attribute]]),
          Seq(outerAcc.asInstanceOf[Operand[Attribute]]),
          outerBodyEntry,
          exit,
        )
      )

      val outerBodyIv = outerBodyEntry.arguments.head
      val outerBodyAcc = outerBodyEntry.arguments(1)
      current = outerBodyEntry
      val savedOuter = mutable.Map.from(valueMap)
      valueMap.addAll(Seq(outerBody.arguments.head -> outerBodyIv, outerBody.arguments(1) -> outerBodyAcc))
      prefixOps.foreach(lowerSimpleOp)
      val prefixValues = prefixOps.flatMap(_.results.map(r => remap(r)))
      valueMap.clear(); valueMap.addAll(savedOuter)
      outerBodyEntry.addOp(
        llvm.Br(
          Seq(
            outerBodyIv.asInstanceOf[Operand[Attribute]],
            asIndex(innerLb),
            outerBodyAcc.asInstanceOf[Operand[Attribute]],
          ) ++ prefixValues.map(_.asInstanceOf[Operand[Attribute]]),
          innerHeader,
        )
      )

      val innerOuterIv = innerHeader.arguments.head
      val innerIv = innerHeader.arguments(1)
      val innerAcc = innerHeader.arguments(2)
      val innerExtras = innerHeader.arguments.drop(3)
      val innerCmp = llvm.ICmp(asIndex(innerIv), asIndex(innerUb), StringData("slt"), Result(I1))
      innerHeader.addOp(innerCmp)
      innerHeader.addOp(
        llvm.CondBr(
          asI1(innerCmp.res),
          Seq(
            innerOuterIv.asInstanceOf[Operand[Attribute]],
            innerIv.asInstanceOf[Operand[Attribute]],
            innerAcc.asInstanceOf[Operand[Attribute]],
          ) ++ innerExtras.map(_.asInstanceOf[Operand[Attribute]]),
          Seq(
            innerOuterIv.asInstanceOf[Operand[Attribute]],
            innerAcc.asInstanceOf[Operand[Attribute]],
          ),
          innerBodyEntry,
          outerLatch,
        )
      )

      current = innerBodyEntry
      val saved = mutable.Map.from(valueMap)
      valueMap.addAll(
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
      valueMap.clear(); valueMap.addAll(saved)
      yielded.foreach { y =>
        val step = emitIndexConstant(op.step.value.value)
        val nextIv = llvm.Add(asIndex(innerBodyEntry.arguments(1)), asIndex(step), Result(IndexType()), Some(overflowNSWNuw))
        emit(nextIv)
        emit(
          llvm.Br(
            Seq(
              innerBodyEntry.arguments.head.asInstanceOf[Operand[Attribute]],
              nextIv.res.asInstanceOf[Operand[Attribute]],
              y.asInstanceOf[Operand[Attribute]],
            ) ++ innerBodyEntry.arguments.drop(3).map(_.asInstanceOf[Operand[Attribute]]),
            innerHeader,
          )
        )
      }

      current = outerLatch
      val outerStep = emitIndexConstant(op.step.value.value)
      val nextOuter = llvm.Add(asIndex(outerLatch.arguments.head), asIndex(outerStep), Result(IndexType()), Some(overflowNSWNuw))
      emit(nextOuter)
      emit(llvm.Br(Seq(nextOuter.res.asInstanceOf[Operand[Attribute]], outerLatch.arguments(1).asInstanceOf[Operand[Attribute]]), outerHeader))

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
        appendBlock(header)
        val body = Block(Seq(IndexType(), init.typ), Seq.empty)
        appendBlock(body)
        val exit = Block(Seq(init.typ), Seq.empty)
        appendBlock(exit)
        emit(llvm.Br(Seq(asIndex(lb), init.asInstanceOf[Operand[Attribute]]), header))
        val iv = header.arguments.head
        val acc = header.arguments(1)
        val cmp = llvm.ICmp(asIndex(iv), asIndex(ub), StringData("slt"), Result(I1))
        header.addOp(cmp)
        header.addOp(
          llvm.CondBr(
            asI1(cmp.res),
            Seq(iv.asInstanceOf[Operand[Attribute]], acc.asInstanceOf[Operand[Attribute]]),
            Seq(acc.asInstanceOf[Operand[Attribute]]),
            body,
            exit,
          )
        )
        current = body
        val saved = mutable.Map.from(valueMap)
        valueMap.addAll(Seq(bodyBlock.arguments.head -> body.arguments.head, bodyBlock.arguments(1) -> body.arguments(1)))
        var yielded: Option[Value[Attribute]] = None
        bodyBlock.operations.foreach {
          case y: Yield => yielded = Some(remap(y.arguments.head))
          case other    => lowerSimpleOp(other)
        }
        valueMap.clear(); valueMap.addAll(saved)
        yielded.foreach { y =>
          val step = emitIndexConstant(op.step.value.value)
          val nextIv = llvm.Add(asIndex(body.arguments.head), asIndex(step), Result(IndexType()), Some(overflowNSWNuw))
          emit(nextIv)
          emit(llvm.Br(Seq(nextIv.res.asInstanceOf[Operand[Attribute]], y.asInstanceOf[Operand[Attribute]]), header))
        }
        current = exit
        exit.arguments.head

  def lower(): func.Func =
    funcOp.body.blocks.head.operations.foreach {
      case loop: For => lowerFor(loop).foreach(v => valueMap(loop.results.head) = v)
      case other     => lowerSimpleOp(other)
    }
    func.Func(funcOp.sym_name, funcOp.function_type, funcOp.sym_visibility, Region(blocks.toSeq))

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists(_.isInstanceOf[For])) =>
    Builder(op).lower()
}

final class LowerBaselineControlFlowToLLVM(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "lower-baseline-control-flow-to-llvm"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))

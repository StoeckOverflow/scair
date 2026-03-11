package scair.passes.d_affine_to_scf

import scair.MLContext
import scair.dialects.affine.*
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.dialects.scf
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

import scala.collection.mutable

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def asScfInt(v: Value[Attribute]): Operand[scf.AnySignlessIntegerOrIndex] =
  v.asInstanceOf[Operand[scf.AnySignlessIntegerOrIndex]]

private def idxConst(v: BigInt): arith.Constant =
  arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

private def projectedMapOperand(
    dimOperands: Seq[Value[Attribute]],
    symbolOperands: Seq[Value[Attribute]],
    map: AffineMapAttr,
): Option[Value[Attribute]] =
  if map.affineMap.affineExprs.size != 1 then None
  else
    val dimNames = map.affineMap.dimensions
    val symNames = map.affineMap.symbols
    map.affineMap.affineExprs.head match
      case AffineDimExpr(position) =>
        val idx = dimNames.indexOf(position)
        if idx < 0 then None else Some(dimOperands(idx))
      case AffineSymExpr(position) =>
        val idx = symNames.indexOf(position)
        if idx < 0 then None else Some(symbolOperands(idx))
      case _ => None

private def lowerSingleResultMap(
    operands: Seq[Value[Attribute]],
    map: AffineMapAttr,
): Option[(Seq[Operation], Value[Attribute])] =
  val dimCount = map.affineMap.dimensions.size
  val symCount = map.affineMap.symbols.size
  if operands.size != dimCount + symCount || map.affineMap.affineExprs.size != 1
  then None
  else
    val dimOperands = operands.take(dimCount)
    val symbolOperands = operands.drop(dimCount).take(symCount)
    projectedMapOperand(dimOperands, symbolOperands, map).map(v => (Seq.empty, v))
      .orElse(
        map.affineMap.affineExprs.head match
          case AffineConstantExpr(value) =>
            val cst = idxConst(value)
            Some((Seq(cst), cst.result))
          case _ => None
      )

private def remapValue(
    value: Value[Attribute],
    valueMapper: mutable.Map[Value[Attribute], Value[Attribute]],
): Value[Attribute] =
  valueMapper.getOrElse(value, value)

private def lowerRegion(
    region: Region,
    outerMapper: mutable.Map[Value[Attribute], Value[Attribute]],
): Region =
  val oldBlock = region.blocks.head
  Region(
    Block(oldBlock.arguments.map(_.typ), newArgs =>
      val localMapper = mutable.Map.from(outerMapper)
      localMapper.addAll(oldBlock.arguments.zip(newArgs))
      oldBlock.operations.flatMap {
        case op: d_affine.For =>
          lowerFor(op, localMapper).toSeq.flatMap((prefix, lowered) =>
            localMapper.addAll(op.results.zip(lowered.results))
            prefix :+ lowered
          )
        case d_affine.Yield(args) =>
          Seq(
            scf.YieldOp(
              args.map(arg => remapValue(arg, localMapper).asInstanceOf[Operand[Attribute]])
            )
          )
        case other =>
          Seq(other.deepCopy(using mutable.Map.empty, localMapper))
      }
    )
  )

private def lowerFor(
    op: d_affine.For,
    valueMapper: mutable.Map[Value[Attribute], Value[Attribute]],
): Option[(Seq[Operation], scf.ForOp)] =
  for
    (lbPrelude, lb) <- lowerSingleResultMap(
      op.lowerBoundOperands.map(v => remapValue(v, valueMapper)),
      op.lowerBoundMap,
    )
    (ubPrelude, ub) <- lowerSingleResultMap(
      op.upperBoundOperands.map(v => remapValue(v, valueMapper)),
      op.upperBoundMap,
    )
  yield
    val stepConst = idxConst(op.step.value.value)
    val lowered = scf.ForOp(
      lowerBound = asScfInt(lb),
      upperBound = asScfInt(ub),
      step = asScfInt(stepConst.result),
      initArgs = op.inits
        .map(v => remapValue(v, valueMapper).asInstanceOf[Operand[Attribute]]),
      region = lowerRegion(op.body, valueMapper),
      resultss = op.res.map(r => Result(r.typ)),
    )
    (lbPrelude ++ ubPrelude ++ Seq(stepConst), lowered)

private val LowerFor = pattern {
  case op: d_affine.For if lowerFor(op, mutable.Map.empty).isDefined =>
    val (prefix, lowered) = lowerFor(op, mutable.Map.empty).get
    (prefix :+ lowered, lowered.results)
}

final class DAffineToSCF(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "d-affine-to-scf"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(LowerFor))
  )

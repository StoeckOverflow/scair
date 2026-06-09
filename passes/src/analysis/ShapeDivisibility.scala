package scair.passes.analysis

import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.ShapeIndexProvenance

import scala.collection.mutable

private enum ShapeDivBound:
  case AnyPositive
  case Finite(d: BigInt)

private object ShapeDivBound:
  val one: ShapeDivBound = Finite(BigInt(1))

  def fromConst(c: BigInt): ShapeDivBound =
    if c == 0 then AnyPositive else Finite(c.abs)

  def gcd(a: ShapeDivBound, b: ShapeDivBound): ShapeDivBound =
    (a, b) match
      case (AnyPositive, x) => x
      case (x, AnyPositive) => x
      case (Finite(x), Finite(y)) =>
        val g = x.gcd(y)
        if g <= 0 then one else Finite(g)

  def mul(a: ShapeDivBound, b: ShapeDivBound): ShapeDivBound =
    (a, b) match
      case (AnyPositive, _) => AnyPositive
      case (_, AnyPositive) => AnyPositive
      case (Finite(x), Finite(y)) =>
        val p = x * y
        if p <= 0 then one else Finite(p)

final class ShapeDivisibilityFacts private (root: Operation):
  private val memo = mutable.Map.empty[Value[Attribute], ShapeDivBound]
  private val inProgress = mutable.Set.empty[Value[Attribute]]

  private def normalize(v: Value[Attribute]): Value[Attribute] =
    v.typ match
      case ValueRefType(ref) => normalize(ref.getVal())
      case _                 => v

  private def inferAffineExpr(
      expr: AffineExpr,
      dimBounds: Map[String, ShapeDivBound],
      symBounds: Map[String, ShapeDivBound],
  ): ShapeDivBound =
    expr match
      case AffineDimExpr(position)   => dimBounds.getOrElse(position, ShapeDivBound.one)
      case AffineSymExpr(position)   => symBounds.getOrElse(position, ShapeDivBound.one)
      case AffineConstantExpr(value) => ShapeDivBound.fromConst(value)
      case AffineBinaryOpExpr(op, lhs, rhs) =>
        op match
          case AffineBinaryOp.Add | AffineBinaryOp.Minus =>
            ShapeDivBound.gcd(
              inferAffineExpr(lhs, dimBounds, symBounds),
              inferAffineExpr(rhs, dimBounds, symBounds),
            )
          case AffineBinaryOp.Multiply =>
            ShapeDivBound.mul(
              inferAffineExpr(lhs, dimBounds, symBounds),
              inferAffineExpr(rhs, dimBounds, symBounds),
            )
          case AffineBinaryOp.CeilDiv | AffineBinaryOp.FloorDiv | AffineBinaryOp.Mod =>
            ShapeDivBound.one

  private def inferAffineApply(
      args: Seq[Value[Attribute]],
      map: AffineMapAttr,
  ): ShapeDivBound =
    val dimNames = map.affineMap.dimensions
    val symNames = map.affineMap.symbols
    val dimCount = dimNames.size
    if map.affineMap.affineExprs.size != 1 then ShapeDivBound.one
    else if args.size != dimCount + symNames.size then ShapeDivBound.one
    else
      val dimBounds = dimNames.zip(args.take(dimCount).map(inferShapeProvenance)).toMap
      val symBounds = symNames.zip(args.drop(dimCount).map(inferShapeProvenance)).toMap
      inferAffineExpr(map.affineMap.affineExprs.head, dimBounds, symBounds)

  private def infer(v: Value[Attribute]): ShapeDivBound =
    val n = normalize(v)
    memo.getOrElseUpdate(
      n, {
        if inProgress.contains(n) then ShapeDivBound.one
        else
          inProgress += n
          val out = n.owner match
            case Some(arith.Constant(IntegerAttr(IntData(c), _: IndexType), _)) =>
              ShapeDivBound.fromConst(c)
            case Some(arith.AddI(lhs, rhs, _, _)) if n.typ == IndexType() =>
              ShapeDivBound.gcd(infer(lhs), infer(rhs))
            case Some(arith.MulI(lhs, rhs, _, _)) if n.typ == IndexType() =>
              ShapeDivBound.mul(infer(lhs), infer(rhs))
            case Some(d_affine.Min(dimOperands, symbolOperands, map, _)) =>
              inferAffineApply(dimOperands ++ symbolOperands, map)
            case Some(d_affine.Apply(dimOperands, symbolOperands, map, _)) =>
              inferAffineApply(dimOperands ++ symbolOperands, map)
            case Some(_: Operation) =>
              ShapeDivBound.one
            case _ =>
              ShapeDivBound.one
          inProgress -= n
          out
      },
    )

  private def inferShapeProvenance(v: Value[Attribute]): ShapeDivBound =
    ShapeIndexProvenance.resolveIndex(v) match
      case Some(index) => infer(index)
      case None        => ShapeDivBound.one

  def isDivisibleBy(v: Value[Attribute], k: BigInt): Boolean =
    if k <= 0 then false
    else
      inferShapeProvenance(v) match
        case ShapeDivBound.AnyPositive => true
        case ShapeDivBound.Finite(d)   => d % k == 0

  def largestDivisibleIn(
      v: Value[Attribute],
      candidates: Seq[Int],
  ): Option[Int] =
    candidates.filter(_ > 0).distinct.sorted.reverse.find(k =>
      isDivisibleBy(v, BigInt(k))
    )

object ShapeDivisibilityFacts:
  def apply(root: Operation): ShapeDivisibilityFacts =
    new ShapeDivisibilityFacts(root)

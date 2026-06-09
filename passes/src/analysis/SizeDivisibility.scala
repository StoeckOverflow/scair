package scair.passes.analysis

import scair.dialects.affine.*
import scair.dialects.builtin.*
import scair.dialects.d_tensor.*
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.SizeWitnessProvenance

import scala.collection.mutable

private enum DivBound:
  case AnyPositive
  case Finite(d: BigInt)

private object DivBound:
  val one: DivBound = Finite(BigInt(1))

  def fromConst(c: BigInt): DivBound =
    if c == 0 then AnyPositive else Finite(c.abs)

  def gcd(a: DivBound, b: DivBound): DivBound =
    (a, b) match
      case (AnyPositive, x) => x
      case (x, AnyPositive) => x
      case (Finite(x), Finite(y)) =>
        val g = x.gcd(y)
        if g <= 0 then one else Finite(g)

  def mul(a: DivBound, b: DivBound): DivBound =
    (a, b) match
      case (AnyPositive, _) => AnyPositive
      case (_, AnyPositive) => AnyPositive
      case (Finite(x), Finite(y)) =>
        val p = x * y
        if p <= 0 then one else Finite(p)

final class SizeDivisibilityFacts private (root: Operation):
  private val memo = mutable.Map.empty[Value[Attribute], DivBound]
  private val inProgress = mutable.Set.empty[Value[Attribute]]

  private def normalize(v: Value[Attribute]): Value[Attribute] =
    v.typ match
      case ValueRefType(ref) => normalize(ref.getVal())
      case _                 => v

  private def inferAffineExpr(
      expr: AffineExpr,
      dimBounds: Map[String, DivBound],
      symBounds: Map[String, DivBound],
  ): DivBound =
    expr match
      case AffineDimExpr(position)   => dimBounds.getOrElse(position, DivBound.one)
      case AffineSymExpr(position)   => symBounds.getOrElse(position, DivBound.one)
      case AffineConstantExpr(value) => DivBound.fromConst(value)
      case AffineBinaryOpExpr(op, lhs, rhs) =>
        op match
          case AffineBinaryOp.Add | AffineBinaryOp.Minus =>
            DivBound.gcd(
              inferAffineExpr(lhs, dimBounds, symBounds),
              inferAffineExpr(rhs, dimBounds, symBounds),
            )
          case AffineBinaryOp.Multiply =>
            DivBound.mul(
              inferAffineExpr(lhs, dimBounds, symBounds),
              inferAffineExpr(rhs, dimBounds, symBounds),
            )
          case AffineBinaryOp.CeilDiv | AffineBinaryOp.FloorDiv | AffineBinaryOp.Mod =>
            DivBound.one

  private def inferAffineApply(
      args: Seq[Value[Attribute]],
      map: AffineMapAttr,
  ): DivBound =
    val dimNames = map.affineMap.dimensions
    val symNames = map.affineMap.symbols
    val dimCount = dimNames.size
    if map.affineMap.affineExprs.size != 1 then DivBound.one
    else if args.size != dimCount + symNames.size then DivBound.one
    else
      val dimBounds = dimNames.zip(args.take(dimCount).map(inferSizeWitnessProvenance)).toMap
      val symBounds = symNames.zip(args.drop(dimCount).map(inferSizeWitnessProvenance)).toMap
      inferAffineExpr(map.affineMap.affineExprs.head, dimBounds, symBounds)

  private def infer(v: Value[Attribute]): DivBound =
    val n = normalize(v)
    memo.getOrElseUpdate(
      n, {
        if inProgress.contains(n) then DivBound.one
        else
          inProgress += n
          val out = n.owner match
            case Some(SizeConstant(IntegerAttr(IntData(c), _), _)) =>
              DivBound.fromConst(c)
            case Some(_: SizeParam) =>
              DivBound.one
            case Some(SizeAdd(lhs, rhs, _)) =>
              DivBound.gcd(infer(lhs), infer(rhs))
            case Some(SizeMul(lhs, rhs, _)) =>
              DivBound.mul(infer(lhs), infer(rhs))
            case Some(d_affine.Min(dimOperands, symbolOperands, map, _)) =>
              inferAffineApply(dimOperands ++ symbolOperands, map)
            case Some(d_affine.Apply(dimOperands, symbolOperands, map, _)) =>
              inferAffineApply(dimOperands ++ symbolOperands, map)
            case Some(_: Operation) =>
              DivBound.one
            case _ =>
              DivBound.one
          inProgress -= n
          out
      },
    )

  private def inferSizeWitnessProvenance(v: Value[Attribute]): DivBound =
    SizeWitnessProvenance.resolveSizeWitness(v) match
      case Some(size) => infer(size)
      case None      => DivBound.one

  def isDivisibleBy(v: Value[Attribute], k: BigInt): Boolean =
    if k <= 0 then false
    else
      inferSizeWitnessProvenance(v) match
        case DivBound.AnyPositive => true
        case DivBound.Finite(d)   => d % k == 0

  def largestDivisibleIn(
      v: Value[Attribute],
      candidates: Seq[Int],
  ): Option[Int] =
    candidates.filter(_ > 0).distinct.sorted.reverse.find(k =>
      isDivisibleBy(v, BigInt(k))
    )

object SizeDivisibilityFacts:
  def apply(root: Operation): SizeDivisibilityFacts =
    new SizeDivisibilityFacts(root)

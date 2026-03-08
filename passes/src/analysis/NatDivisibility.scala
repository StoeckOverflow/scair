package scair.passes.analysis

import scair.dialects.dTensor.*
import scair.dialects.d_affine
import scair.dialects.builtin.*
import scair.ir.*
import scair.utils.OK

import scala.collection.mutable

private enum DivBound:
  case AnyPositive // exact zero: divisible by every positive integer
  case Finite(d: BigInt) // guaranteed positive divisor lower bound

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

final class NatDivisibilityFacts private (root: Operation):

  private val memo = mutable.Map.empty[Value[Attribute], DivBound]
  private val inProgress = mutable.Set.empty[Value[Attribute]]

  private def normalize(v: Value[Attribute]): Value[Attribute] =
    v.typ match
      case ValueRefType(ref) => normalize(ref.getVal())
      case _                 => v

  private def infer(v: Value[Attribute]): DivBound =
    val n = normalize(v)
    memo.getOrElseUpdate(
      n, {
        if inProgress.contains(n) then DivBound.one
        else
          inProgress += n
          val out = n.owner match
            case Some(
                  NatConst(
                    IntegerAttr(IntData(c), _),
                    _,
                  )
                ) =>
              DivBound.fromConst(c)
            case Some(_: NatParam) => DivBound.one
            case Some(NatAdd(lhs, rhs, _)) =>
              DivBound.gcd(infer(lhs), infer(rhs))
            case Some(NatMul(lhs, rhs, _)) =>
              DivBound.mul(infer(lhs), infer(rhs))
            case Some(ShapeToIndex(nat, _)) =>
              infer(nat)
            case Some(d_affine.Min(lhs, rhs, _)) =>
              DivBound.gcd(inferNatProvenance(lhs), inferNatProvenance(rhs))
            case Some(_: Operation) =>
              DivBound.one
            case _ =>
              DivBound.one
          inProgress -= n
          out
      },
    )

  private def inferNatProvenance(v: Value[Attribute]): DivBound =
    dTensorTypeUtil.resolveNatProvenance(v) match
      case OK(nat) => infer(nat)
      case _       => DivBound.one

  def guaranteedDivisor(v: Value[Attribute]): BigInt =
    inferNatProvenance(v) match
      case DivBound.AnyPositive => BigInt(0)
      case DivBound.Finite(d)   => d

  def isDivisibleBy(v: Value[Attribute], k: BigInt): Boolean =
    if k <= 0 then false
    else
      inferNatProvenance(v) match
        case DivBound.AnyPositive => true
        case DivBound.Finite(d)   => d % k == 0

  def largestDivisibleIn(
      v: Value[Attribute],
      candidates: Seq[Int],
  ): Option[Int] =
    candidates.filter(_ > 0).distinct.sorted.reverse.find(k =>
      isDivisibleBy(v, BigInt(k))
    )

object NatDivisibilityFacts:
  def apply(root: Operation): NatDivisibilityFacts =
    new NatDivisibilityFacts(root)

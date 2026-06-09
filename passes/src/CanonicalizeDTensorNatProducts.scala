package scair.passes.canonicalize_d_tensor_nat_products

import scair.MLContext
import scair.dialects.{d_tensor as DTensor}
import scair.ir.*
import scair.passes.analysis.NatProductFacts
import scair.transformations.{GreedyRewritePatternApplier, PatternAction, PatternRewriteWalker, WalkerPass, pattern}

private def factorRank(f: NatProductFacts.Factor): (Int, BigInt) =
  f.constValue match
    case Some(k) if f.value.owner.exists(_.isInstanceOf[DTensor.NatConst]) => (0, k)
    case _                                                                => (1, BigInt(0))

private def isCanonical(factors: Seq[NatProductFacts.Factor]): Boolean =
  factors == factors.sortBy(factorRank)

private def resultTypeFor(
    lhs: Value[Attribute],
    rhs: Value[Attribute],
    finalType: DTensor.DTensorNatLikeType,
    isFinal: Boolean,
): DTensor.DTensorNatLikeType =
  if isFinal then finalType
  else if lhs.typ.isInstanceOf[DTensor.DTensorPosNatType] &&
      rhs.typ.isInstanceOf[DTensor.DTensorPosNatType]
  then DTensor.DTensorPosNatType()
  else DTensor.DTensorNatType()

private def buildProduct(
    factors: Seq[NatProductFacts.Factor],
    finalType: DTensor.DTensorNatLikeType,
): (Seq[Operation], Value[Attribute]) =
  var prelude = Seq.empty[Operation]
  var acc = factors.head.value
  factors.tail.zipWithIndex.foreach { case (factor, idx) =>
    val isFinal = idx == factors.tail.size - 1
    val mul = DTensor.NatMul(
      acc.asInstanceOf[Operand[DTensor.DTensorNatLikeType]],
      factor.value.asInstanceOf[Operand[DTensor.DTensorNatLikeType]],
      Result(resultTypeFor(acc, factor.value, finalType, isFinal)),
    )
    prelude = prelude :+ mul
    acc = mul.res
  }
  (prelude, acc)

private val CanonicalizeNatMul = pattern { case op: DTensor.NatMul =>
  NatProductFacts.factorMultiset(op.res).filter(product =>
    product.factors.size > 1 && !isCanonical(product.factors)
  ) match
    case Some(product) =>
      val ordered = product.factors.sortBy(factorRank)
      val (prelude, result) = buildProduct(ordered, op.res.typ)
      (prelude, result)
    case None => PatternAction.Abort
}

final class CanonicalizeDTensorNatProducts(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "canonicalize-d-tensor-nat-products"

  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(CanonicalizeNatMul)))

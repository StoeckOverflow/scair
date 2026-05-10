package scair.passes.canonicalize_dtensor_nat_products

import scair.MLContext
import scair.dialects.dTensor
import scair.ir.*
import scair.passes.analysis.NatProductFacts
import scair.transformations.{GreedyRewritePatternApplier, PatternAction, PatternRewriteWalker, WalkerPass, pattern}

private def factorRank(f: NatProductFacts.Factor): (Int, BigInt) =
  f.constValue match
    case Some(k) if f.value.owner.exists(_.isInstanceOf[dTensor.NatConst]) => (0, k)
    case _                                                                => (1, BigInt(0))

private def isCanonical(factors: Seq[NatProductFacts.Factor]): Boolean =
  factors == factors.sortBy(factorRank)

private def resultTypeFor(
    lhs: Value[Attribute],
    rhs: Value[Attribute],
    finalType: dTensor.dTensorNatLikeType,
    isFinal: Boolean,
): dTensor.dTensorNatLikeType =
  if isFinal then finalType
  else if lhs.typ.isInstanceOf[dTensor.dTensorPosNatType] &&
      rhs.typ.isInstanceOf[dTensor.dTensorPosNatType]
  then dTensor.dTensorPosNatType()
  else dTensor.dTensorNatType()

private def buildProduct(
    factors: Seq[NatProductFacts.Factor],
    finalType: dTensor.dTensorNatLikeType,
): (Seq[Operation], Value[Attribute]) =
  var prelude = Seq.empty[Operation]
  var acc = factors.head.value
  factors.tail.zipWithIndex.foreach { case (factor, idx) =>
    val isFinal = idx == factors.tail.size - 1
    val mul = dTensor.NatMul(
      acc.asInstanceOf[Operand[dTensor.dTensorNatLikeType]],
      factor.value.asInstanceOf[Operand[dTensor.dTensorNatLikeType]],
      Result(resultTypeFor(acc, factor.value, finalType, isFinal)),
    )
    prelude = prelude :+ mul
    acc = mul.res
  }
  (prelude, acc)

private val CanonicalizeNatMul = pattern { case op: dTensor.NatMul =>
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
  override val name: String = "canonicalize-dtensor-nat-products"

  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(CanonicalizeNatMul)))

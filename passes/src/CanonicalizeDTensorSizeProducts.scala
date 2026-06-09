package scair.passes.canonicalize_d_tensor_size_products

import scair.MLContext
import scair.dialects.{d_tensor as DTensor}
import scair.ir.*
import scair.passes.analysis.SizeProductFacts
import scair.transformations.{GreedyRewritePatternApplier, PatternAction, PatternRewriteWalker, WalkerPass, pattern}

private def factorRank(f: SizeProductFacts.Factor): (Int, BigInt) =
  f.constValue match
    case Some(k) if f.value.owner.exists(_.isInstanceOf[DTensor.SizeConstant]) => (0, k)
    case _                                                                => (1, BigInt(0))

private def isCanonical(factors: Seq[SizeProductFacts.Factor]): Boolean =
  factors == factors.sortBy(factorRank)

private def resultTypeFor(
    lhs: Value[Attribute],
    rhs: Value[Attribute],
    finalType: DTensor.DTensorSizeWitnessType,
    isFinal: Boolean,
): DTensor.DTensorSizeWitnessType =
  if isFinal then finalType
  else if lhs.typ.isInstanceOf[DTensor.DTensorPosSizeType] &&
      rhs.typ.isInstanceOf[DTensor.DTensorPosSizeType]
  then DTensor.DTensorPosSizeType()
  else DTensor.DTensorSizeType()

private def buildProduct(
    factors: Seq[SizeProductFacts.Factor],
    finalType: DTensor.DTensorSizeWitnessType,
): (Seq[Operation], Value[Attribute]) =
  var prelude = Seq.empty[Operation]
  var acc = factors.head.value
  factors.tail.zipWithIndex.foreach { case (factor, idx) =>
    val isFinal = idx == factors.tail.size - 1
    val mul = DTensor.SizeMul(
      acc.asInstanceOf[Operand[DTensor.DTensorSizeWitnessType]],
      factor.value.asInstanceOf[Operand[DTensor.DTensorSizeWitnessType]],
      Result(resultTypeFor(acc, factor.value, finalType, isFinal)),
    )
    prelude = prelude :+ mul
    acc = mul.res
  }
  (prelude, acc)

private val CanonicalizeSizeMul = pattern { case op: DTensor.SizeMul =>
  SizeProductFacts.factorMultiset(op.res).filter(product =>
    product.factors.size > 1 && !isCanonical(product.factors)
  ) match
    case Some(product) =>
      val ordered = product.factors.sortBy(factorRank)
      val (prelude, result) = buildProduct(ordered, op.res.typ)
      (prelude, result)
    case None => PatternAction.Abort
}

final class CanonicalizeDTensorSizeProducts(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "canonicalize-d-tensor-size-products"

  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(CanonicalizeSizeMul)))

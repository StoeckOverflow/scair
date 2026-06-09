package scair.passes.canonicalize_d_tensor_shape_products

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.ir.*
import scair.passes.analysis.ShapeProductFacts
import scair.transformations.{GreedyRewritePatternApplier, PatternAction, PatternRewriteWalker, WalkerPass, pattern}

private def factorRank(f: ShapeProductFacts.Factor): (Int, BigInt) =
  f.constValue match
    case Some(k) if f.value.owner.exists(_.isInstanceOf[arith.Constant]) => (0, k)
    case _                                                              => (1, BigInt(0))

private def isCanonical(factors: Seq[ShapeProductFacts.Factor]): Boolean =
  factors == factors.sortBy(factorRank)

private def buildProduct(
    factors: Seq[ShapeProductFacts.Factor],
): (Seq[Operation], Value[Attribute]) =
  var prelude = Seq.empty[Operation]
  var acc = factors.head.value
  factors.tail.foreach { factor =>
    val mul = arith.MulI(
      acc.asInstanceOf[Operand[arith.AnyIntegerType]],
      factor.value.asInstanceOf[Operand[arith.AnyIntegerType]],
      Result(IndexType()),
    )
    prelude = prelude :+ mul
    acc = mul.result
  }
  (prelude, acc)

private val CanonicalizeShapeProduct = pattern { case op: arith.MulI =>
  ShapeProductFacts.factorMultiset(op.result).filter(product =>
    product.factors.size > 1 && !isCanonical(product.factors)
  ) match
    case Some(product) =>
      val ordered = product.factors.sortBy(factorRank)
      val (prelude, result) = buildProduct(ordered)
      (prelude, result)
    case None => PatternAction.Abort
}

final class CanonicalizeDTensorShapeProducts(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "canonicalize-d-tensor-shape-products"

  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(CanonicalizeShapeProduct)))

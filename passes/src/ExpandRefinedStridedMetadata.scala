package scair.passes.expand_refined_strided_metadata

import scair.MLContext
import scair.dialects.d_memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

private val ExpandReinterpret = pattern {
  case op: d_memref.ReinterpretCast
      if !op.src.owner.exists(_.isInstanceOf[d_memref.ExtractStridedMetadata]) =>
    val extract = d_memref.ExtractStridedMetadata.build(op.src)
    val base = extract.results.head.asInstanceOf[Operand[d_memref.dMemrefMemrefType]]
    val replacement = d_memref.ReinterpretCast(
      base,
      op.offset,
      op.sizes,
      op.strides,
      op.res,
    )
    (Seq(extract, replacement), replacement.results)
}

final class ExpandRefinedStridedMetadata(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "expand-refined-strided-metadata"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(Seq(ExpandReinterpret))
    )

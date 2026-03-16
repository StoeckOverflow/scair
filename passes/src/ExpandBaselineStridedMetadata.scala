package scair.passes.expand_baseline_strided_metadata

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

private val ExpandReinterpret = pattern {
  case op: memref.ReinterpretCast
      if !op.src.owner.exists(_.isInstanceOf[memref.ExtractStridedMetadata]) =>
    val extract = memref.ExtractStridedMetadata.build(op.src)
    val base = extract.results.head.asInstanceOf[Operand[MemrefType]]
    val replacement = memref.ReinterpretCast(
      base,
      op.offset,
      op.sizes,
      op.strides,
      op.res,
    )
    (Seq(extract, replacement), replacement.results)
}

final class ExpandBaselineStridedMetadata(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "expand-baseline-strided-metadata"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(Seq(ExpandReinterpret))
    )

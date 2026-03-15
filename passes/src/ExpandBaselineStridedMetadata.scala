package scair.passes.expand_baseline_strided_metadata

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.llvm
import scair.dialects.memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

private def descriptorType(rank: Int): llvm.StructType =
  llvm.StructType(
    Seq(
      llvm.Ptr(),
      llvm.Ptr(),
      IndexType(),
      llvm.ArrayType(IntData(rank), IndexType()),
      llvm.ArrayType(IntData(rank), IndexType()),
    )
  )

private val ExpandAlloc = pattern {
  case op: memref.Alloc =>
    val ty = op.memref.typ.asInstanceOf[RankedMemrefType]
    memref.DescriptorAlloc(
      op.dynamicSizes,
      Result(descriptorType(ty.shape.attrValues.size)),
      op.memref.typ,
    )
}

private val ExpandReinterpret = pattern {
  case op: memref.ReinterpretCast =>
    val ty = op.res.typ.asInstanceOf[RankedMemrefType]
    memref.DescriptorReinterpret(
      Seq(op.src.asInstanceOf[Operand[Attribute]], op.offset.asInstanceOf[Operand[Attribute]]) ++
        op.sizes.map(_.asInstanceOf[Operand[Attribute]]) ++
        op.strides.map(_.asInstanceOf[Operand[Attribute]]),
      Result(descriptorType(ty.shape.attrValues.size)),
      op.src.typ,
      op.res.typ,
    )
}

private val ExpandLoad = pattern {
  case op: memref.Load =>
    memref.DescriptorLoad(
      Seq(op.memref.asInstanceOf[Operand[Attribute]]) ++ op.indices.map(_.asInstanceOf[Operand[Attribute]]),
      Result(op.result.typ),
      op.memref.typ,
    )
}

private val ExpandDealloc = pattern {
  case op: memref.Dealloc =>
    memref.DescriptorDealloc(op.memref.asInstanceOf[Operand[Attribute]])
}

final class ExpandBaselineStridedMetadata(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "expand-baseline-strided-metadata"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(Seq(ExpandAlloc, ExpandReinterpret, ExpandLoad, ExpandDealloc))
    )

package scair.passes.expand_refined_strided_metadata

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.d_memref
import scair.dialects.llvm
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
  case op: d_memref.Alloc =>
    llvm.RefinedAllocDescriptor(
      Seq.empty,
      Result(descriptorType(op.res.typ.params.size)),
      op.res.typ,
    )
}

private val ExpandReinterpret = pattern {
  case op: d_memref.ReinterpretCast =>
    llvm.RefinedReinterpretDescriptor(
      (Seq(op.src.asInstanceOf[Operand[Attribute]], op.offset.asInstanceOf[Operand[Attribute]]) ++
        op.sizes.map(_.asInstanceOf[Operand[Attribute]]) ++
        op.strides.map(_.asInstanceOf[Operand[Attribute]])),
      Result(descriptorType(op.res.typ.params.size)),
      op.src.typ,
      op.res.typ,
    )
}

private val ExpandLoad = pattern {
  case op: d_memref.Load =>
    llvm.RefinedLoad(
      Seq(op.memref.asInstanceOf[Operand[Attribute]]) ++ op.indices.map(_.asInstanceOf[Operand[Attribute]]),
      Result(op.res.typ),
      op.memref.typ,
    )
}

private val ExpandDealloc = pattern {
  case op: d_memref.Dealloc =>
    llvm.RefinedDealloc(op.memref.asInstanceOf[Operand[Attribute]])
}

final class ExpandRefinedStridedMetadata(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "expand-refined-strided-metadata"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(Seq(ExpandAlloc, ExpandReinterpret, ExpandLoad, ExpandDealloc))
    )

package scair.passes.dtensor_to_dmemref

import scair.MLContext
import scair.dialects.builtin.UnrealizedConversionCastOp
import scair.dialects.dTensor.*
import scair.dialects.d_memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

object DTensorDMemrefConversion:
  def tensorToMemrefType(t: dTensorTensorType): d_memref.dMemrefMemrefType =
    d_memref.dMemrefMemrefType(t.params, t.elem)

  def toMemrefValue(
      t: Value[Attribute],
      asType: d_memref.dMemrefMemrefType,
  ): (Seq[Operation], Value[Attribute]) =
    val cast = UnrealizedConversionCastOp(
      inputs = Seq(t),
      outputs = Seq(Result(asType)),
    )
    (Seq(cast), cast.outputs.head)

private val LowerDim = pattern {
  case dtensorDim @ Dim(t, axis, res) =>
    val memType = DTensorDMemrefConversion.tensorToMemrefType(t.typ)
    val (prefix, memValue) = DTensorDMemrefConversion.toMemrefValue(t, memType)
    val lowered = d_memref.Dim(
      memValue.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
      axis,
      Result(res.typ),
    )
    (prefix :+ lowered, Seq(lowered.res))
}

final class DTensorToDMemrefShapePreserving(ctx: MLContext)
    extends WalkerPass(ctx):
  /** Scope (intentionally narrow):
    *   - lower `dtensor.dim` into `d_memref.dim` while preserving exact dim SSA identity
    *   - use unrealized cast as a bridge from `!dtensor.tensor` to `!d_memref.memref`
    *
    * This pass is not a full dtensor->d_memref conversion.
    */
  override val name: String = "dtensor-to-dmemref-shape-preserving"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(LowerDim))
  )

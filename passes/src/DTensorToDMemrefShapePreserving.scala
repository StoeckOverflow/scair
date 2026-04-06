package scair.passes.dtensor_to_dmemref

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.ir.*
import scair.transformations.{GreedyRewritePatternApplier, PatternRewriteWalker, WalkerPass, pattern}

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

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def asMemref(
    v: Value[Attribute]
): Operand[d_memref.dMemrefMemrefType] =
  v.asInstanceOf[Operand[d_memref.dMemrefMemrefType]]

private def idxConst(v: Int): arith.Constant =
  arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

private def toIndex(nat: Value[Attribute]): ShapeToIndex =
  ShapeToIndex(nat.asInstanceOf[Operand[dTensorNatType]], Result(IndexType()))

private def identityMap: AffineMapAttr =
  AffineMapAttr(
    AffineMap(
      dimensions = Seq("d0"),
      symbols = Seq.empty,
      affineExprs = Seq(AffineDimExpr("d0")),
    )
  )

private def mkFor(
    lb: Value[Attribute],
    ub: Value[Attribute],
)(
    bodyBuilder: Value[Attribute] => Seq[Operation]
): d_affine.For =
  val body = Region(
    Block(IndexType(), iv => bodyBuilder(iv) :+ d_affine.Yield(Seq.empty))
  )
  d_affine.For(
    lowerBoundOperands = Seq(asIndex(lb)),
    upperBoundOperands = Seq(asIndex(ub)),
    inits = Seq.empty,
    res = Seq.empty,
    lowerBoundMap = identityMap,
    upperBoundMap = identityMap,
    step = IntegerAttr(IntData(1), I32),
    body = body,
  )

private def castBackToTensor(
    memref: Value[Attribute],
    tensorType: dTensorTensorType,
): UnrealizedConversionCastOp =
  UnrealizedConversionCastOp(
    inputs = Seq(memref),
    outputs = Seq(Result(tensorType)),
  )

private def buildFillNest(
    memref: Value[Attribute],
    fillValue: Operand[TypeAttribute],
    dimIdxs: Seq[Value[Attribute]],
    zero: Value[Attribute],
    ivs: Seq[Value[Attribute]] = Seq.empty,
): Seq[Operation] =
  dimIdxs match
    case Seq() =>
      Seq(
        d_memref.Store(
          fillValue,
          asMemref(memref),
          ivs.map(asIndex),
        )
      )
    case head +: tail =>
      Seq(
        mkFor(zero, head) { iv =>
          buildFillNest(memref, fillValue, tail, zero, ivs :+ iv)
        }
      )

private val LowerEmpty = pattern {
  case Empty(res) =>
    val memTy = DTensorDMemrefConversion.tensorToMemrefType(res.typ)
    val alloc = d_memref.Alloc(Result(memTy))
    val castBack = castBackToTensor(alloc.res, res.typ)
    (Seq(alloc, castBack), Seq(castBack.outputs.head))
}

private val LowerFill = pattern {
  case Fill(v, res) =>
    val memTy = DTensorDMemrefConversion.tensorToMemrefType(res.typ)
    val idxDims = res.typ.params.map(_.getVal()).map(toIndex)
    val zero = idxConst(0)
    val alloc = d_memref.Alloc(Result(memTy))
    val fillOps = buildFillNest(alloc.res, v, idxDims.map(_.res), zero.result)
    val castBack = castBackToTensor(alloc.res, res.typ)
    (Seq(zero) ++ idxDims ++ Seq(alloc) ++ fillOps ++ Seq(castBack), Seq(castBack.outputs.head))
}

private val LowerDim = pattern {
  case dtensorDim @ Dim(t, axis, res) =>
    val memType = DTensorDMemrefConversion.tensorToMemrefType(t.typ)
    val (prefix, memValue) = DTensorDMemrefConversion.toMemrefValue(t, memType)
    val lowered = d_memref.DimExact(
      memValue.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
      axis,
      Result(res.typ),
    )
    (prefix :+ lowered, Seq(lowered.res))
}

private val LowerCast = pattern {
  case Cast(src, res) =>
    val srcMemType = DTensorDMemrefConversion.tensorToMemrefType(src.typ)
    val resMemType = DTensorDMemrefConversion.tensorToMemrefType(res.typ)
    val (prefix, memValue) = DTensorDMemrefConversion.toMemrefValue(src, srcMemType)
    val lowered = d_memref.Cast(
      memValue.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
      Result(resMemType),
    )
    val castBack = castBackToTensor(lowered.res, res.typ)
    (prefix ++ Seq(lowered, castBack), Seq(castBack.outputs.head))
}

/**
 * Lowers a small shape-preserving subset of `dtensor` ops to `d_memref` while
 * keeping tensor-typed SSA results via unrealized casts.
 *
 * This pass rewrites `dtensor.empty` to buffer allocation, rewrites
 * `dtensor.fill` to allocate-and-store loop nests, rewrites `dtensor.dim` to
 * `d_memref.dim_exact`, and rewrites `dtensor.cast` to `d_memref.cast`. In each
 * case where a tensor result must remain visible, it bridges back with
 * `unrealized_conversion_cast` so value-dependent shapes are preserved in the
 * underlying `!d_memref.memref`.
 *
 * Rewrite shapes:
 * `<dtensor.empty -> !dtensor.tensor<...>>`
 * `->`
 * `<d_memref.alloc + unrealized_conversion_cast to !dtensor.tensor<...>>`
 *
 * `<dtensor.fill %value -> !dtensor.tensor<...>>`
 * `->`
 * `<shape-to-index setup + d_memref.alloc + nested d_affine.for + d_memref.store + unrealized_conversion_cast>`
 *
 * `<dtensor.dim %tensor, %axis>`
 * `->`
 * `<unrealized_conversion_cast to memref + d_memref.dim_exact>`
 *
 * `<dtensor.cast %src : !dtensor.tensor<...> to !dtensor.tensor<...>>`
 * `->`
 * `<unrealized_conversion_cast to source memref + d_memref.cast + unrealized_conversion_cast back to tensor>`
 */
final class DTensorToDMemrefShapePreserving(ctx: MLContext)
    extends WalkerPass(ctx):
  /** Scope (intentionally narrow):
    *   - lower a minimal executable subset: `dtensor.empty`, `dtensor.fill`,
    *     `dtensor.cast`, and `dtensor.dim`
    *   - preserve value-dependent shapes in `!d_memref.memref`
    *   - use unrealized casts as temporary bridges between `!dtensor.tensor`
    *     and `!d_memref.memref`
    *
    * This pass is still not a full dtensor->d_memref conversion.
    */
  override val name: String = "dtensor-to-dmemref-shape-preserving"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(LowerEmpty, LowerFill, LowerDim, LowerCast))
  )

package scair.passes.dtensor_to_dmemref

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.ir.*
import scair.transformations.{
  GreedyRewritePatternApplier,
  Owner,
  PatternAction,
  PatternRewriteWalker,
  WalkerPass,
  pattern,
}

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
  ShapeToIndex(nat.asInstanceOf[Operand[dTensorNatLikeType]], Result(IndexType()))

private def layoutParamValue(param: d_memref.LayoutParam): Option[Value[Attribute]] =
  param match
    case v: ValueAttribute => Some(v.getVal())
    case _                 => None

private def layoutFromValue(v: Value[Attribute]): d_memref.LayoutParam =
  ValueAttribute(v)

private def layoutOne: d_memref.LayoutParam =
  IntegerAttr(IntData(1), IndexType())

private def layoutZero: d_memref.LayoutParam =
  IntegerAttr(IntData(0), IndexType())

private def intAttrValue(attr: IntegerAttr): BigInt = attr.value.value

private def multiplyLayoutByIndex(
    lhs: d_memref.LayoutParam,
    rhs: Value[Attribute],
): (Seq[Operation], d_memref.LayoutParam) =
  lhs match
    case IntegerAttr(IntData(1), _: IndexType | _: IntegerType) =>
      (Seq.empty, layoutFromValue(rhs))
    case _ =>
      layoutParamValue(lhs) match
        case Some(lhsValue) =>
          val mul = arith.MulI(
            lhsValue.asInstanceOf[Operand[arith.AnyIntegerType]],
            rhs.asInstanceOf[Operand[arith.AnyIntegerType]],
            Result(IndexType()),
          )
          (Seq(mul), layoutFromValue(mul.result))
        case None =>
          throw new IllegalArgumentException(
            "dtensor expand_shape lowering only supports unit literal or SSA layout strides"
          )

private def buildSourceRowMajorStrides(
    dimIdxValues: Seq[Value[Attribute]]
): (Seq[Operation], Seq[d_memref.LayoutParam]) =
  val strides = Array.fill[d_memref.LayoutParam](dimIdxValues.size)(layoutOne)
  var current: d_memref.LayoutParam = layoutOne
  var ops = Seq.empty[Operation]
  for i <- dimIdxValues.indices.reverse do
    strides(i) = current
    if i > 0 then
      val (nextOps, nextCurrent) = multiplyLayoutByIndex(current, dimIdxValues(i))
      ops = ops ++ nextOps
      current = nextCurrent
  (ops, strides.toSeq)

private def reassociationGroups(
    reassociation: ArrayAttribute[Attribute]
): Seq[Seq[Int]] =
  reassociation.attrValues.map {
    case group: ArrayAttribute[?] =>
      group.attrValues.map {
        case IntegerAttr(IntData(idx), _) => idx.toInt
        case other =>
          throw new IllegalArgumentException(
            s"dtensor expand_shape lowering expected integer reassociation index, got ${dTensorTypeUtil.renderAttr(other)}"
          )
      }
    case other =>
      throw new IllegalArgumentException(
        s"dtensor expand_shape lowering expected array reassociation group, got ${dTensorTypeUtil.renderAttr(other)}"
      )
  }

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
    stepOperands = Seq.empty,
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

private val LowerExpandShape = pattern {
  case ExpandShape(src, reassociation, res) =>
    val srcMemType = DTensorDMemrefConversion.tensorToMemrefType(src.typ)
    val (prefix, memValue) = DTensorDMemrefConversion.toMemrefValue(src, srcMemType)
    val srcDimIdxs = src.typ.params.map(_.getVal()).map(toIndex)
    val resDimIdxs = res.typ.params.map(_.getVal()).map(toIndex)
    val (srcStrideOps, srcStrides) =
      buildSourceRowMajorStrides(srcDimIdxs.map(_.res))
    val groups = reassociationGroups(reassociation)
    val resultStrides = Array.fill[d_memref.LayoutParam](res.typ.params.size)(layoutOne)
    var splitStrideOps = Seq.empty[Operation]

    groups.zipWithIndex.foreach { case (group, srcIdx) =>
      var currentStride = srcStrides(srcIdx)
      group.reverse.foreach { resIdx =>
        resultStrides(resIdx) = currentStride
        if resIdx != group.head then
          val (ops, nextStride) =
            multiplyLayoutByIndex(currentStride, resDimIdxs(resIdx).res)
          splitStrideOps = splitStrideOps ++ ops
          currentStride = nextStride
      }
    }

    val resMemType = d_memref.dMemrefMemrefType(
      res.typ.params,
      res.typ.elem,
      offset = Some(layoutZero),
      strides = Some(resultStrides.toSeq),
    )
    val reinterpret = d_memref.ReinterpretCast(
      memValue.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
      Result(resMemType),
    )
    val castBack = castBackToTensor(reinterpret.res, res.typ)
    (
      prefix ++ srcDimIdxs ++ resDimIdxs ++ srcStrideOps ++ splitStrideOps ++
        Seq(reinterpret, castBack),
      Seq(castBack.outputs.head),
    )
}

private def isPermutation(
    attr: ArrayAttribute[Attribute],
    expected: Seq[Int],
): Boolean =
  attr.attrValues == expected.map(i => IntegerAttr(IntData(i), I32))

private val LowerExact2DTiledView = pattern {
  case PermuteDims(Owner(splitN: SplitDim), permutation, res)
      if intAttrValue(splitN.dim) == 2 &&
        isPermutation(permutation, Seq(0, 2, 1, 3)) =>
    splitN.src.owner match
      case Some(splitM: SplitDim) if intAttrValue(splitM.dim) == 0 =>
        val srcTy = splitM.src.typ
        val splitMTy = splitM.res.typ
        val splitNTy = splitN.res.typ
        val srcRank = srcTy.params.size
        val splitMRank = splitMTy.params.size
        val splitNRank = splitNTy.params.size
        val resRank = res.typ.params.size

        if srcRank == 2 && splitMRank == 3 && splitNRank == 4 && resRank == 4 then
          val srcMemType = DTensorDMemrefConversion.tensorToMemrefType(srcTy)
          val (prefix, memValue) =
            DTensorDMemrefConversion.toMemrefValue(splitM.src, srcMemType)

          val tmToIndex = toIndex(splitMTy.params(1).getVal())
          val nToIndex = toIndex(srcTy.params(1).getVal())
          val tnToIndex = toIndex(splitNTy.params(3).getVal())
          val tmTimesN = arith.MulI(
            tmToIndex.res.asInstanceOf[Operand[arith.AnyIntegerType]],
            nToIndex.res.asInstanceOf[Operand[arith.AnyIntegerType]],
            Result(IndexType()),
          )

          val resMemType = d_memref.dMemrefMemrefType(
            res.typ.params,
            res.typ.elem,
            offset = Some(layoutZero),
            strides = Some(
              Seq(
                layoutFromValue(tmTimesN.result),
                layoutFromValue(tnToIndex.res),
                layoutFromValue(nToIndex.res),
                layoutOne,
              )
            ),
          )
          val reinterpret = d_memref.ReinterpretCast(
            memValue.asInstanceOf[Operand[d_memref.dMemrefMemrefType]],
            Result(resMemType),
          )
          val castBack = castBackToTensor(reinterpret.res, res.typ)
          (
            prefix ++ Seq(tmToIndex, nToIndex, tnToIndex, tmTimesN, reinterpret, castBack),
            Seq(castBack.outputs.head),
          )
        else PatternAction.Abort
      case _ => PatternAction.Abort
}

/**
 * Lowers a small shape-preserving subset of `dtensor` ops to `d_memref` while
 * keeping tensor-typed SSA results via unrealized casts.
 *
 * This pass rewrites `dtensor.empty` to buffer allocation, rewrites
 * `dtensor.fill` to allocate-and-store loop nests, rewrites `dtensor.dim` to
 * `d_memref.dim_exact`, rewrites `dtensor.cast` to `d_memref.cast`, and rewrites
 * generic row-major `dtensor.expand_shape` metadata to `d_memref.reinterpret_cast`.
 * In each case where a tensor result must remain visible, it bridges back with
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
 *
 * `<dtensor.expand_shape %src : !dtensor.tensor<[..., product], ...> to !dtensor.tensor<[..., factors...], ...>>`
 * `->`
 * `<unrealized_conversion_cast to source memref + d_memref.reinterpret_cast with row-major expanded strides + unrealized_conversion_cast back to tensor>`
 *
 * `<dtensor.split_dim dim 0; dtensor.split_dim dim 2; dtensor.permute_dims [0,2,1,3]>`
 * `->`
 * `<unrealized_conversion_cast to original source memref + d_memref.reinterpret_cast with logical tiled-view strides [tm * n, tn, n, 1] + unrealized_conversion_cast back to tensor>`
 */
final class DTensorToDMemrefShapePreserving(ctx: MLContext)
    extends WalkerPass(ctx):
  /** Scope (intentionally narrow):
    *   - lower a minimal executable subset: `dtensor.empty`, `dtensor.fill`,
    *     `dtensor.cast`, `dtensor.dim`, and generic row-major
    *     `dtensor.expand_shape`
    *   - lower the exact 2D logical tiled-view shape pattern
    *     `split_dim dim 0`, `split_dim dim 2`, `permute_dims [0,2,1,3]`
    *   - preserve value-dependent shapes in `!d_memref.memref`
    *   - use unrealized casts as temporary bridges between `!dtensor.tensor`
    *     and `!d_memref.memref`
    *
    * This pass is still not a full dtensor->d_memref conversion.
    */
  override val name: String = "dtensor-to-dmemref-shape-preserving"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(
      Seq(
        LowerEmpty,
        LowerFill,
        LowerDim,
        LowerCast,
        LowerExpandShape,
        LowerExact2DTiledView,
      )
    )
  )

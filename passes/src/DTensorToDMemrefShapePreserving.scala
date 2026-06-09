package scair.passes.d_tensor_to_d_memref

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_tensor.*
import scair.dialects.d_affine
import scair.dialects.d_memref
import scair.ir.*
import scair.utils.OK
import scair.transformations.{
  GreedyRewritePatternApplier,
  PatternAction,
  PatternRewriteWalker,
  WalkerPass,
  pattern,
}

object DTensorDMemrefConversion:
  def tensorToMemrefType(t: DTensorTensorType): d_memref.DMemrefMemrefType =
    d_memref.DMemrefMemrefType(t.params, t.elem)

  def toMemrefValue(
      t: Value[Attribute],
      asType: d_memref.DMemrefMemrefType,
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
): Operand[d_memref.DMemrefMemrefType] =
  v.asInstanceOf[Operand[d_memref.DMemrefMemrefType]]

private def idxConst(v: Int): arith.Constant =
  arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

private def toIndex(nat: Value[Attribute]): ShapeToIndex =
  ShapeToIndex(nat.asInstanceOf[Operand[DTensorNatLikeType]], Result(IndexType()))

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
    tensorType: DTensorTensorType,
): UnrealizedConversionCastOp =
  UnrealizedConversionCastOp(
    inputs = Seq(memref),
    outputs = Seq(Result(tensorType)),
  )

private def parseReassociationGroups(
    reassociation: ArrayAttribute[Attribute]
): Option[Seq[Seq[Int]]] =
  reassociation.attrValues.foldLeft[Option[Seq[Seq[Int]]]](Some(Seq.empty)) {
    case (acc, group: ArrayAttribute[?]) =>
      acc.flatMap(groups =>
        group.attrValues.foldLeft[Option[Seq[Int]]](Some(Seq.empty)) {
          case (groupAcc, IntegerAttr(IntData(idx), I32)) =>
            groupAcc.map(_ :+ idx.toInt)
          case _ => None
        }.map(indices => groups :+ indices)
      )
    case _ => None
  }

private def productMatches(
    product: Value[Attribute],
    factors: Seq[Value[Attribute]],
): Boolean =
  DTensorTypeUtil.sameOrderedNatProduct(product, factors) match
    case OK(true) => true
    case _        => false

private def productType(
    lhs: Value[Attribute],
    rhs: Value[Attribute],
): DTensorNatLikeType =
  (lhs.typ, rhs.typ) match
    case (_: DTensorPosNatType, _: DTensorPosNatType) => DTensorPosNatType()
    case _                                            => DTensorNatType()

private def buildOrderedProduct(
    dims: Seq[Value[Attribute]]
): (Seq[Operation], d_memref.LayoutParam) =
  dims match
    case Seq() =>
      (Seq.empty, IntegerAttr(IntData(1), IndexType()))
    case Seq(dim) =>
      (Seq.empty, ValueAttribute(dim))
    case first +: rest =>
      val (ops, product) = rest.foldLeft((Seq.empty[Operation], first)) {
        case ((ops, acc), dim) =>
          val mul = NatMul(
            acc.asInstanceOf[Operand[DTensorNatLikeType]],
            dim.asInstanceOf[Operand[DTensorNatLikeType]],
            Result(productType(acc, dim)),
          )
          (ops :+ mul, mul.res)
      }
      (ops, ValueAttribute(product))

private def rowMajorMemrefType(
    tensorType: DTensorTensorType
): (Seq[Operation], d_memref.DMemrefMemrefType) =
  val dims = tensorType.params.map(_.getVal())
  val (strideOps, strides) =
    dims.indices.foldLeft((Seq.empty[Operation], Seq.empty[d_memref.LayoutParam])) {
      case ((ops, ss), idx) =>
        val (productOps, stride) = buildOrderedProduct(dims.drop(idx + 1))
        (ops ++ productOps, ss :+ stride)
    }
  val memType = d_memref.DMemrefMemrefType(
    tensorType.params,
    tensorType.elem,
    Some(IntegerAttr(IntData(0), IndexType())),
    Some(strides),
  )
  (strideOps, memType)

private def lowerReinterpretView(
    src: Operand[DTensorTensorType],
    resType: DTensorTensorType,
): (Seq[Operation], Seq[Value[Attribute]]) =
  val srcMemType = DTensorDMemrefConversion.tensorToMemrefType(src.typ)
  val (prefix, memValue) = DTensorDMemrefConversion.toMemrefValue(src, srcMemType)
  val (strideOps, resMemType) = rowMajorMemrefType(resType)
  val reinterpret = d_memref.ReinterpretCast(
    memValue.asInstanceOf[Operand[d_memref.DMemrefMemrefType]],
    Result(resMemType),
  )
  val castBack = castBackToTensor(reinterpret.res, resType)
  (prefix ++ strideOps ++ Seq(reinterpret, castBack), Seq(castBack.outputs.head))

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
  case d_tensorDim @ Dim(t, axis, res) =>
    val memType = DTensorDMemrefConversion.tensorToMemrefType(t.typ)
    val (prefix, memValue) = DTensorDMemrefConversion.toMemrefValue(t, memType)
    val lowered = d_memref.DimExact(
      memValue.asInstanceOf[Operand[d_memref.DMemrefMemrefType]],
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
      memValue.asInstanceOf[Operand[d_memref.DMemrefMemrefType]],
      Result(resMemType),
    )
    val castBack = castBackToTensor(lowered.res, res.typ)
    (prefix ++ Seq(lowered, castBack), Seq(castBack.outputs.head))
}

private val LowerCollapseShape = pattern {
  case op: CollapseShape =>
    parseReassociationGroups(op.reassociation) match
      case Some(groups)
          if groups.size == op.res.typ.params.size &&
            groups.zipWithIndex.forall { case (group, resIdx) =>
              productMatches(
                op.res.typ.params(resIdx).getVal(),
                group.map(srcIdx => op.src.typ.params(srcIdx).getVal()),
              )
            } =>
        lowerReinterpretView(op.src, op.res.typ)
      case _ => PatternAction.Abort
}

private val LowerJoinDim = pattern {
  case op: JoinDim =>
    val axis = op.dim.value.value
    val srcRank = op.src.typ.params.size
    if axis < 0 || axis + 1 >= srcRank then PatternAction.Abort
    else
      val idx = axis.toInt
      if productMatches(
          op.res.typ.params(idx).getVal(),
          Seq(op.src.typ.params(idx).getVal(), op.src.typ.params(idx + 1).getVal()),
        )
      then lowerReinterpretView(op.src, op.res.typ)
      else PatternAction.Abort
}

private val LowerExpandShape = pattern {
  case op: ExpandShape =>
    parseReassociationGroups(op.reassociation) match
      case Some(groups)
          if groups.size == op.src.typ.params.size &&
            groups.zipWithIndex.forall { case (group, srcIdx) =>
              productMatches(
                op.src.typ.params(srcIdx).getVal(),
                group.map(resIdx => op.res.typ.params(resIdx).getVal()),
              )
            } =>
        lowerReinterpretView(op.src, op.res.typ)
      case _ => PatternAction.Abort
}

private val LowerSplitDim = pattern {
  case op: SplitDim =>
    val axis = op.dim.value.value
    val srcRank = op.src.typ.params.size
    if axis < 0 || axis >= srcRank then PatternAction.Abort
    else
      val idx = axis.toInt
      if productMatches(
          op.src.typ.params(idx).getVal(),
          Seq(op.outer, op.inner),
        )
      then lowerReinterpretView(op.src, op.res.typ)
      else PatternAction.Abort
}

/**
 * Lowers a small shape-preserving subset of `d_tensor` ops to `d_memref` while
 * keeping tensor-typed SSA results via unrealized casts.
 *
 * This pass rewrites `d_tensor.empty` to buffer allocation, rewrites
 * `d_tensor.fill` to allocate-and-store loop nests, rewrites `d_tensor.dim` to
 * `d_memref.dim_exact`, and rewrites `d_tensor.cast` to `d_memref.cast`.
 * In each case where a tensor result must remain visible, it bridges back with
 * `unrealized_conversion_cast` so value-dependent shapes are preserved in the
 * underlying `!d_memref.memref`.
 *
 * Rewrite shapes:
 * `<d_tensor.empty -> !d_tensor.tensor<...>>`
 * `->`
 * `<d_memref.alloc + unrealized_conversion_cast to !d_tensor.tensor<...>>`
 *
 * `<d_tensor.fill %value -> !d_tensor.tensor<...>>`
 * `->`
 * `<shape-to-index setup + d_memref.alloc + nested d_affine.for + d_memref.store + unrealized_conversion_cast>`
 *
 * `<d_tensor.dim %tensor, %axis>`
 * `->`
 * `<unrealized_conversion_cast to memref + d_memref.dim_exact>`
 *
 * `<d_tensor.cast %src : !d_tensor.tensor<...> to !d_tensor.tensor<...>>`
 * `->`
 * `<unrealized_conversion_cast to source memref + d_memref.cast + unrealized_conversion_cast back to tensor>`
 */
final class DTensorToDMemrefShapePreserving(ctx: MLContext)
    extends WalkerPass(ctx):
  /** Scope (intentionally narrow):
    *   - lower a minimal executable subset: `d_tensor.empty`, `d_tensor.fill`,
    *     `d_tensor.cast`, and `d_tensor.dim`
    *   - preserve value-dependent shapes in `!d_memref.memref`
    *   - use unrealized casts as temporary bridges between `!d_tensor.tensor`
    *     and `!d_memref.memref`
    *
    * This pass is still not a full d_tensor->d_memref conversion.
    */
  override val name: String = "d-tensor-to-d-memref-shape-preserving"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(
      Seq(
        LowerEmpty,
        LowerFill,
        LowerDim,
        LowerCast,
        LowerCollapseShape,
        LowerJoinDim,
        LowerExpandShape,
        LowerSplitDim,
      )
    )
  )

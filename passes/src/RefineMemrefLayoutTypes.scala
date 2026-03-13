package scair.passes.refine_memref_layout_types

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.dialects.dTensor.*
import scair.dialects.d_memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

private def idxAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), IndexType())

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def asNat(v: Value[Attribute]): Operand[dTensorNatType] =
  v.asInstanceOf[Operand[dTensorNatType]]

private def constIndexValue(v: Value[Attribute]): Option[BigInt] =
  v.owner match
    case Some(arith.Constant(IntegerAttr(IntData(k), _: IndexType), _)) => Some(k)
    case _                                                              => None

private def asLayoutValue(v: Value[Attribute]): d_memref.LayoutParam =
  ValueAttribute(v)

private case class LayoutExpr(
    prefix: Vector[Operation],
    param: d_memref.LayoutParam,
)

private object LayoutExpr:
  val zero: LayoutExpr = LayoutExpr(Vector.empty, idxAttr(0))
  val one: LayoutExpr = LayoutExpr(Vector.empty, idxAttr(1))

private def layoutValueToIndex(
    param: d_memref.LayoutParam
): LayoutExpr =
  param match
    case i: IntegerAttr =>
      LayoutExpr(Vector.empty, i)
    case v: ValueAttribute =>
      v.getVal().typ match
        case _: IndexType =>
          LayoutExpr(Vector.empty, v)
        case _: dTensorNatType =>
          val cast = ShapeToIndex(asNat(v.getVal()), Result(IndexType()))
          LayoutExpr(Vector(cast), asLayoutValue(cast.res))
        case ValueRefType(ref) =>
          layoutValueToIndex(ValueAttribute(ref.getVal()))

private def materializeIndexValue(expr: LayoutExpr): (Vector[Operation], Value[Attribute]) =
  expr.param match
    case v: ValueAttribute => (expr.prefix, v.getVal())
    case IntegerAttr(IntData(k), _) =>
      val cst = arith.Constant(idxAttr(k), Result(IndexType()))
      (expr.prefix :+ cst, cst.result)

private def multiplyIndex(
    lhs: Value[Attribute],
    rhs: LayoutExpr,
): LayoutExpr =
  (constIndexValue(lhs), rhs.param) match
    case (Some(0), _)                         => LayoutExpr.zero
    case (Some(1), _)                         => rhs
    case (_, IntegerAttr(IntData(0), _))      => LayoutExpr.zero
    case (_, IntegerAttr(IntData(1), _))      => LayoutExpr(Vector.empty, asLayoutValue(lhs))
    case (Some(a), IntegerAttr(IntData(b), _)) => LayoutExpr(Vector.empty, idxAttr(a * b))
    case _ =>
      val rhsIndex = layoutValueToIndex(rhs.param)
      val (rhsPrefix, rhsValue) = materializeIndexValue(rhsIndex)
      val mul = arith.MulI(asIndex(lhs), asIndex(rhsValue), Result(IndexType()))
      LayoutExpr(rhs.prefix ++ rhsPrefix :+ mul, asLayoutValue(mul.result))

private def addLayout(
    lhs: LayoutExpr,
    rhs: LayoutExpr,
): LayoutExpr =
  (lhs.param, rhs.param) match
    case (IntegerAttr(IntData(0), _), _) => rhs.copy(prefix = lhs.prefix ++ rhs.prefix)
    case (_, IntegerAttr(IntData(0), _)) => lhs.copy(prefix = lhs.prefix ++ rhs.prefix)
    case (IntegerAttr(IntData(a), _), IntegerAttr(IntData(b), _)) =>
      LayoutExpr(lhs.prefix ++ rhs.prefix, idxAttr(a + b))
    case _ =>
      val lhsIndex = layoutValueToIndex(lhs.param)
      val rhsIndex = layoutValueToIndex(rhs.param)
      val (lhsPrefix, lhsValue) = materializeIndexValue(lhsIndex)
      val (rhsPrefix, rhsValue) = materializeIndexValue(rhsIndex)
      val add = arith.AddI(asIndex(lhsValue), asIndex(rhsValue), Result(IndexType()))
      LayoutExpr(
        lhs.prefix ++ rhs.prefix ++ lhsPrefix ++ rhsPrefix :+ add,
        asLayoutValue(add.result),
      )

private def defaultRowMajorStride(
    ty: d_memref.dMemrefMemrefType,
    axis: Int,
): LayoutExpr =
  ty.params.drop(axis + 1).foldLeft(LayoutExpr.one) { case (acc, dim) =>
    multiplyIndex(dim.getVal(), acc)
  }

private def sourceOffset(
    ty: d_memref.dMemrefMemrefType
): LayoutExpr =
  ty.offset match
    case Some(off) => layoutValueToIndex(off)
    case None      => LayoutExpr.zero

private def sourceStride(
    ty: d_memref.dMemrefMemrefType,
    axis: Int,
): LayoutExpr =
  ty.strides match
    case Some(ss) => layoutValueToIndex(ss(axis))
    case None     => defaultRowMajorStride(ty, axis)

private def refineSubviewType(
    op: d_memref.Subview
): (Vector[Operation], d_memref.dMemrefMemrefType) =
  val srcTy = op.src.typ
  val srcOffset = sourceOffset(srcTy)

  val offsetExpr =
    op.offsets.zipWithIndex.foldLeft(srcOffset) { case (acc, (off, axis)) =>
      addLayout(acc, multiplyIndex(off, sourceStride(srcTy, axis)))
    }

  val strideExprs =
    op.strides.zipWithIndex.map { case (subStride, axis) =>
      multiplyIndex(subStride, sourceStride(srcTy, axis))
    }

  val prefix = (offsetExpr.prefix ++ strideExprs.flatMap(_.prefix)).toVector
  val refinedType = d_memref.dMemrefMemrefType(
    params = op.res.typ.params,
    elem = op.res.typ.elem,
    offset = Some(offsetExpr.param),
    strides = Some(strideExprs.map(_.param)),
  )
  (prefix, refinedType)

private def refineReinterpretCastType(
    op: d_memref.ReinterpretCast
): (Vector[Operation], d_memref.dMemrefMemrefType) =
  val refinedType = d_memref.dMemrefMemrefType(
    params = op.res.typ.params,
    elem = op.res.typ.elem,
    offset = Some(asLayoutValue(op.offset)),
    strides = Some(op.strides.map(asLayoutValue)),
  )
  (Vector.empty, refinedType)

private val RefineSubviewLayout = pattern {
  case op: d_memref.Subview =>
    if op.res.typ.offset.nonEmpty && op.res.typ.strides.nonEmpty then
      PatternAction.Abort
    else
      val (prefix, refinedType) = refineSubviewType(op)
      val refined = d_memref.Subview(
        op.src,
        op.offsets,
        op.sizes,
        op.strides,
        Result(refinedType),
      )
      (prefix :+ refined, Seq(refined.res))
}

private val RefineReinterpretLayout = pattern {
  case op: d_memref.ReinterpretCast =>
    if op.res.typ.offset.nonEmpty && op.res.typ.strides.nonEmpty then
      PatternAction.Abort
    else
      val (prefix, refinedType) = refineReinterpretCastType(op)
      val refined = d_memref.ReinterpretCast(
        op.src,
        op.offset,
        op.sizes,
        op.strides,
        Result(refinedType),
      )
      (prefix :+ refined, Seq(refined.res))
}

final class RefineMemrefLayoutTypes(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "refine-memref-layout-types"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(RefineSubviewLayout, RefineReinterpretLayout))
  )

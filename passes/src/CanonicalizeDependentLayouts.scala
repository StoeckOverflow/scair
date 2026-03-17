package scair.passes.canonicalize_dependent_layouts

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor.ShapeToIndex
import scair.dialects.d_memref
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

private def constIntValue(v: Value[Attribute]): Option[IntegerAttr] =
  v.owner match
    case Some(arith.Constant(attr: IntegerAttr, _)) => Some(attr)
    case _                                          => None

private def isConst(v: Value[Attribute], k: BigInt): Boolean =
  constIntValue(v).exists(_.value.value == k)

private def simplifyLayoutParam(
    param: d_memref.LayoutParam
): d_memref.LayoutParam =
  param match
    case v: ValueAttribute =>
      v.getVal().typ match
        case ValueRefType(ref) =>
          simplifyLayoutParam(ValueAttribute(ref.getVal()))
        case _ =>
          v.getVal().owner match
            case Some(arith.Constant(attr: IntegerAttr, _)) =>
              attr
            case Some(arith.AddI(lhs, rhs, _)) if isConst(lhs, 0) =>
              simplifyLayoutParam(ValueAttribute(rhs))
            case Some(arith.AddI(lhs, rhs, _)) if isConst(rhs, 0) =>
              simplifyLayoutParam(ValueAttribute(lhs))
            case Some(arith.MulI(lhs, _, _)) if isConst(lhs, 0) =>
              IntegerAttr(IntData(0), IndexType())
            case Some(arith.MulI(_, rhs, _)) if isConst(rhs, 0) =>
              IntegerAttr(IntData(0), IndexType())
            case Some(arith.MulI(lhs, rhs, _)) if isConst(lhs, 1) =>
              simplifyLayoutParam(ValueAttribute(rhs))
            case Some(arith.MulI(lhs, rhs, _)) if isConst(rhs, 1) =>
              simplifyLayoutParam(ValueAttribute(lhs))
            case _ =>
              v
    case other => other

private def simplifyMemrefType(
    ty: d_memref.dMemrefMemrefType
): d_memref.dMemrefMemrefType =
  val newOffset = ty.offset.map(simplifyLayoutParam)
  val newStrides = ty.strides.map(_.map(simplifyLayoutParam))
  if newOffset == ty.offset && newStrides == ty.strides then ty
  else
    d_memref.dMemrefMemrefType(
      params = ty.params,
      elem = ty.elem,
      offset = newOffset,
      strides = newStrides,
    )

private val SimplifyDependentLayouts = pattern {
  case op if op.results.exists(_.typ.isInstanceOf[d_memref.dMemrefMemrefType]) =>
    val newTypes = op.results.map { result =>
      result.typ match
        case ty: d_memref.dMemrefMemrefType => simplifyMemrefType(ty)
        case other                          => other
    }
    if newTypes.zip(op.results).forall((newTy, oldRes) => newTy == oldRes.typ) then
      PatternAction.Abort
    else
      val rewritten = op.updated(results = newTypes.map(ty => Result(ty.asInstanceOf[Attribute])))
      (rewritten, rewritten.results)
}

final class CanonicalizeDependentLayouts(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "canonicalize-dependent-layouts"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(SimplifyDependentLayouts))
  )

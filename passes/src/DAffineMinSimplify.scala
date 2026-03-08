package scair.passes.d_affine_min_simplify

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor.dTensorTypeUtil
import scair.dialects.d_affine
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*
import scair.utils.OK

private def mkIndexConst(v: BigInt): arith.Constant =
  arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

private def sameNatProvenance(lhs: Value[Attribute], rhs: Value[Attribute]): Boolean =
  (dTensorTypeUtil.resolveNatProvenance(lhs), dTensorTypeUtil.resolveNatProvenance(rhs)) match
    case (OK(ln), OK(rn)) => ln eq rn
    case _                => false

private val SimplifyMin = pattern {
  case d_affine.Min(lhs, rhs, _) if lhs eq rhs =>
    (Seq(), Seq(lhs))

  case d_affine.Min(Owner(arith.Constant(IntegerAttr(IntData(0), _), _)), _, _) =>
    mkIndexConst(0)
  case d_affine.Min(_, Owner(arith.Constant(IntegerAttr(IntData(0), _), _)), _) =>
    mkIndexConst(0)

  case d_affine.Min(
        Owner(arith.Constant(IntegerAttr(IntData(a), _), _)),
        Owner(arith.Constant(IntegerAttr(IntData(b), _), _)),
        _,
      ) =>
    mkIndexConst(a.min(b))

  case d_affine.Min(
        Owner(
          scair.dialects.dTensor.ShapeToIndex(
            Owner(scair.dialects.dTensor.NatConst(IntegerAttr(IntData(a), _), _)),
            _,
          )
        ),
        Owner(
          scair.dialects.dTensor.ShapeToIndex(
            Owner(scair.dialects.dTensor.NatConst(IntegerAttr(IntData(b), _), _)),
            _,
          )
        ),
        _,
      ) =>
    mkIndexConst(a.min(b))

  case d_affine.Min(lhs, rhs, _) if sameNatProvenance(lhs, rhs) =>
    (Seq(), Seq(lhs))
}

final class DAffineMinSimplify(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "d-affine-min-simplify"

  override val walker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(SimplifyMin))
  )

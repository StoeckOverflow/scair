package scair.passes.d_affine_min_simplify

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.dialects.d_affine
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

private def mkNatConst(v: BigInt): NatConst =
  NatConst(IntegerAttr(IntData(v), I32), Result(dTensorNatType()))

private val SimplifyMin = pattern {
  case d_affine.Min(lhs, rhs, _) if lhs eq rhs =>
    (Seq(), Seq(lhs))

  case d_affine.Min(Owner(NatConst(IntegerAttr(IntData(0), _), _)), _, _) =>
    mkNatConst(0)
  case d_affine.Min(_, Owner(NatConst(IntegerAttr(IntData(0), _), _)), _) =>
    mkNatConst(0)

  case d_affine.Min(
        Owner(NatConst(IntegerAttr(IntData(a), _), _)),
        Owner(NatConst(IntegerAttr(IntData(b), _), _)),
        _,
      ) =>
    mkNatConst(a.min(b))
}

final class DAffineMinSimplify(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "d-affine-min-simplify"

  override val walker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(SimplifyMin))
  )

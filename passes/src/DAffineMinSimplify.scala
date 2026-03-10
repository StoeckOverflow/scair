package scair.passes.d_affine_min_simplify

import scair.MLContext
import scair.dialects.affine.*
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.analysis.NatProvenance
import scair.transformations.*
import scair.transformations.patterns.*

private def mkIndexConst(v: BigInt): arith.Constant =
  arith.Constant(IntegerAttr(IntData(v), IndexType()), Result(IndexType()))

private def projectedApplyArg(
    dimOperands: Seq[Value[Attribute]],
    symbolOperands: Seq[Value[Attribute]],
    map: AffineMapAttr,
): Option[Value[Attribute]] =
  // Keep this to direct dim/sym projection in the currently supported affine subset.
  if map.affineMap.affineExprs.size != 1 then None
  else
    val dimNames = map.affineMap.dimensions
    val symNames = map.affineMap.symbols
    if dimOperands.size != dimNames.size || symbolOperands.size != symNames.size
    then None
    else
      map.affineMap.affineExprs.head match
        case AffineDimExpr(position) =>
          val idx = dimNames.indexOf(position)
          if idx < 0 then None else Some(dimOperands(idx))
        case AffineSymExpr(position) =>
          val idx = symNames.indexOf(position)
          if idx < 0 then None else Some(symbolOperands(idx))
        case _ => None

private val SimplifyMin = pattern {
  case d_affine.Min(dimOperands, symbolOperands, map, _) if projectedApplyArg(
        dimOperands,
        symbolOperands,
        map,
      ).isDefined =>
    (Seq(), Seq(projectedApplyArg(dimOperands, symbolOperands, map).get))

  case op @ d_affine.Min(_, _, _, _) if NatProvenance.exactConst(op.res).isDefined =>
    mkIndexConst(NatProvenance.exactConst(op.res).get)
}

private val SimplifyApply = pattern {
  // Intentionally limited to direct projection and exact-constant reconstruction from provenance.
  case d_affine.Apply(dimOperands, symbolOperands, map, _) if projectedApplyArg(
        dimOperands,
        symbolOperands,
        map,
      ).isDefined =>
    (Seq(), Seq(projectedApplyArg(dimOperands, symbolOperands, map).get))

  case op @ d_affine.Apply(_, _, _, _) if NatProvenance.exactConst(op.res).isDefined =>
    mkIndexConst(NatProvenance.exactConst(op.res).get)
}

final class DAffineMinSimplify(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "d-affine-min-simplify"

  override val walker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(SimplifyMin, SimplifyApply))
  )

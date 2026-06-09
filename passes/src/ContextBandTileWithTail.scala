package scair.passes.context_band_tiling

import scair.MLContext
import scair.ir.Operation
import scair.passes.analysis.ShapeProductFacts.FactorSelectionPolicy
import scair.passes.tiling.ValueDependentTiling
import scair.transformations.ModulePass

final class OrdinaryAffineContextBandTileWithTail(ctx: MLContext, tileSize: BigInt)
    extends ModulePass(ctx):
  override val name: String = "ordinary-affine-context-band-tile-with-tail"

  override def transform(op: Operation): Operation =
    ValueDependentTiling.transformAffineContextGuarded(op, tileSize)

final class DependentContextBandTileWithTail(ctx: MLContext, tileSize: BigInt)
    extends ModulePass(ctx):
  override val name: String = "dependent-context-band-tile-with-tail"

  override def transform(op: Operation): Operation =
    ValueDependentTiling.transformDAffineContextGuarded(op, tileSize)

final class DependentContextBandExactTile(
    ctx: MLContext,
    factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
) extends ModulePass(ctx):
  override val name: String = "dependent-context-band-exact-tile"

  override def transform(op: Operation): Operation =
    ValueDependentTiling.transformDAffineContextNatmul(
      op,
      factorPolicy,
      ValueDependentTiling.TailMode.Exact,
    )

final class DependentContextBandFactorTileWithTail(
    ctx: MLContext,
    factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
) extends ModulePass(ctx):
  override val name: String = "dependent-context-band-factor-tile-with-tail"

  override def transform(op: Operation): Operation =
    ValueDependentTiling.transformDAffineContextNatmul(
      op,
      factorPolicy,
      ValueDependentTiling.TailMode.Guarded,
    )

final class DependentContextBandSeparableTile(
    ctx: MLContext,
    factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
) extends ModulePass(ctx):
  override val name: String = "dependent-context-band-separable-tile"

  override def transform(op: Operation): Operation =
    ValueDependentTiling.transformDAffineContextNatmul(
      op,
      factorPolicy,
      ValueDependentTiling.TailMode.Separable,
    )

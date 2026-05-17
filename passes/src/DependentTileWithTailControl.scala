package scair.passes.dependent_natmul_tiling

import scair.MLContext
import scair.ir.Operation
import scair.passes.analysis.NatProductFacts.FactorSelectionPolicy
import scair.transformations.ModulePass

final class DependentTileWithTailControl(
    ctx: MLContext,
    factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
) extends ModulePass(ctx):
  override val name: String = "dependent-tile-with-tail-control"

  override def transform(op: Operation): Operation =
    DependentNatmulTilingTransform.transform(op, TailPolicy.Guarded, factorPolicy)

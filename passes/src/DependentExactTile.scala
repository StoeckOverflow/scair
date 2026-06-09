package scair.passes.dependent_size_product_tiling

import scair.MLContext
import scair.ir.Operation
import scair.passes.analysis.SizeProductFacts.FactorSelectionPolicy
import scair.transformations.ModulePass

final class DependentExactTile(
    ctx: MLContext,
    factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
) extends ModulePass(ctx):
  override val name: String = "dependent-exact-tile"

  override def transform(op: Operation): Operation =
    DependentSizeProductTilingTransform.transform(op, TailPolicy.Exact, factorPolicy)

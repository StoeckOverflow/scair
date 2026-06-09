package scair.passes.dependent_product_tiling

import scair.MLContext
import scair.ir.Operation
import scair.passes.analysis.ShapeProductFacts.FactorSelectionPolicy
import scair.transformations.ModulePass

final class DependentProductLoopExactTile(
    ctx: MLContext,
    factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
) extends ModulePass(ctx):
  override val name: String = "dependent-product-loop-exact-tile"

  override def transform(op: Operation): Operation =
    DependentProductTilingTransform.transform(
      op,
      TailPolicy.Exact,
      factorPolicy,
      ProductLoopKind.AnyProductLoop,
    )

final class DependentProductLoopSeparableTile(
    ctx: MLContext,
    factorPolicy: FactorSelectionPolicy = FactorSelectionPolicy.RightmostPositive,
) extends ModulePass(ctx):
  override val name: String = "dependent-product-loop-separable-tile"

  override def transform(op: Operation): Operation =
    DependentProductTilingTransform.transformSeparableWhenNotExact(
      op,
      factorPolicy,
      ProductLoopKind.AnyProductLoop,
    )

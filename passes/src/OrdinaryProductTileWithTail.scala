package scair.passes.dependent_product_tiling

import scair.MLContext
import scair.ir.Operation
import scair.transformations.ModulePass

final class OrdinaryProductTileWithTail(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "ordinary-product-tile-with-tail"

  override def transform(op: Operation): Operation =
    DependentProductTilingTransform.transformOrdinaryIndexProduct(op)

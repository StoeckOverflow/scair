package scair.passes.dependent_natmul_tiling

import scair.MLContext
import scair.ir.Operation
import scair.transformations.ModulePass

final class OrdinaryProductTileWithTail(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "ordinary-product-tile-with-tail"

  private val attributes = DependentNatmulTilingAttributes(
    modeKey = "scair.ordinary_product_tile_with_tail.mode",
    mode = "dynamic_step_tail_guarded",
    generatedKey = "scair.ordinary_product_tile_with_tail.generated",
    tailFreeKey = "scair.ordinary_product_tile_with_tail.tail_free",
    tailFree = "false",
    proofKey = "scair.ordinary_product_tile_with_tail.proof",
    proof = "none",
  )

  override def transform(op: Operation): Operation =
    DependentNatmulTilingTransform.transformOrdinaryIndexProduct(op, attributes)

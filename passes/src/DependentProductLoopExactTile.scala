package scair.passes.dependent_natmul_tiling

import scair.MLContext
import scair.ir.Operation
import scair.transformations.ModulePass

final class DependentProductLoopExactTile(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "dependent-product-loop-exact-tile"

  private val attributes = DependentNatmulTilingAttributes(
    modeKey = "scair.dependent_product_loop_exact_tile.mode",
    mode = "dynamic_step_tail_free",
    generatedKey = "scair.dependent_product_loop_exact_tile.generated",
    tailFreeKey = "scair.dependent_product_loop_exact_tile.tail_free",
    tailFree = "true",
    proofKey = "scair.dependent_product_loop_exact_tile.proof",
    proof = "dtensor.nat.mul",
  )

  override def transform(op: Operation): Operation =
    DependentNatmulTilingTransform.transform(
      op,
      TailPolicy.Exact,
      attributes,
      ProductLoopKind.AnyProductLoop,
    )

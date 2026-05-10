package scair.passes.dependent_natmul_tiling

import scair.MLContext
import scair.ir.Operation
import scair.transformations.ModulePass

final class DependentTileWithTailControl(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "dependent-tile-with-tail-control"

  private val attributes = DependentNatmulTilingAttributes(
    modeKey = "scair.dependent_tile_with_tail_control.mode",
    mode = "dynamic_step_tail_guarded",
    generatedKey = "scair.dependent_tile_with_tail_control.generated",
    tailFreeKey = "scair.dependent_tile_with_tail_control.tail_free",
    tailFree = "false",
    proofKey = "scair.dependent_tile_with_tail_control.proof",
    proof = "none",
  )

  override def transform(op: Operation): Operation =
    DependentNatmulTilingTransform.transform(op, TailPolicy.Guarded, attributes)

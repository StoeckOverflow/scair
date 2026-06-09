package scair.passes.dependent_size_product_tiling

import scair.MLContext
import scair.ir.Operation
import scair.passes.tiling.ValueDependentTiling
import scair.transformations.ModulePass

final class OrdinaryAffineProductTileWithTail(ctx: MLContext, tileSize: BigInt) extends ModulePass(ctx):
  override val name: String = "ordinary-affine-product-tile-with-tail"

  override def transform(op: Operation): Operation =
    ValueDependentTiling.transformAffineProductGuarded(
      op,
      tileSize,
      requireReductionLoop = true,
    )

final class OrdinaryAffineProductLoopTileWithTail(ctx: MLContext, tileSize: BigInt) extends ModulePass(ctx):
  override val name: String = "ordinary-affine-product-loop-tile-with-tail"

  override def transform(op: Operation): Operation =
    ValueDependentTiling.transformAffineProductGuarded(
      op,
      tileSize,
      requireReductionLoop = false,
    )

package scair.passes.validate_d_affine_dynamic_steps

import scair.MLContext
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.ShapeIndexProvenance
import scair.transformations.ModulePass

final class ValidateDAffineDynamicSteps(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "validate-d-affine-dynamic-steps"

  private def visit(op: Operation): Unit =
    op match
      case loop: d_affine.For =>
        loop.stepOperands.headOption.foreach { step =>
          if !ShapeIndexProvenance.isPositive(step) then
            throw new Exception(
              s"d_affine.for dynamic step must be proven strictly positive before lowering"
            )
        }
      case _ => ()
    op.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))

  override def transform(op: Operation): Operation =
    visit(op)
    op

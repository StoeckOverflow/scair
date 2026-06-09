package scair.passes.validate_refined_control_flow_lowerable

import scair.MLContext
import scair.dialects.d_affine
import scair.dialects.func
import scair.dialects.scf
import scair.ir.*
import scair.passes.ShapeIndexProvenance
import scair.passes.control_flow_helpers.explainUnsupporteDAffineMap
import scair.transformations.ModulePass

final class ValidateRefinedControlFlowLowerable(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "validate-refined-control-flow-lowerable"

  private def fail(reason: String): Nothing =
    throw new Exception(
      s"lower-refined-control-flow-to-llvm cannot lower current IR: $reason. " +
        "Use d-affine-to-affine-compatible when the IR should enter the stock affine pipeline, " +
        "or simplify unsupported d_affine bounds before refined CFG lowering."
    )

  private def validateAffineMap(kind: String, map: scair.dialects.builtin.AffineMapAttr): Unit =
    explainUnsupporteDAffineMap(map).foreach(reason => fail(s"$kind has unsupported affine map: $reason"))

  private def validateLoop(loop: d_affine.For): Unit =
    if loop.body.blocks.size != 1 then fail("d_affine.for body must be single-block")
    validateAffineMap("d_affine.for lower bound", loop.lowerBoundMap)
    validateAffineMap("d_affine.for upper bound", loop.upperBoundMap)
    loop.stepOperands.headOption.foreach { step =>
      if !ShapeIndexProvenance.isPositive(step) then
        fail("d_affine.for dynamic step is not proven strictly positive")
    }
    if loop.inits.isEmpty && loop.res.nonEmpty then
      fail("d_affine.for with results must have matching iter_args")
    else if loop.res.size > 1 && loop.inits.size != loop.res.size then
      fail("multi-result d_affine.for must have one init per result")
    loop.body.blocks.foreach(_.operations.foreach(validateOp))

  private def validateIf(ifOp: scf.IfOp): Unit =
    if ifOp.thenRegion.blocks.size != 1 || ifOp.elseRegion.blocks.size != 1 then
      fail("scf.if regions must both be single-block")
    ifOp.thenRegion.blocks.foreach(_.operations.foreach(validateOp))
    ifOp.elseRegion.blocks.foreach(_.operations.foreach(validateOp))

  private def validateFunc(funcOp: func.Func): Unit =
    if funcOp.body.blocks.size > 1 &&
      funcOp.body.blocks.exists(_.operations.exists {
        case _: d_affine.For | _: d_affine.Apply | _: d_affine.Min | _: scf.IfOp => true
        case _                                                                   => false
      })
    then fail("func.func body must be single-block before refined CFG lowering")
    funcOp.body.blocks.foreach(_.operations.foreach(validateOp))

  private def validateOp(op: Operation): Unit =
    op match
      case f: func.Func       => validateFunc(f)
      case loop: d_affine.For => validateLoop(loop)
      case apply: d_affine.Apply =>
        validateAffineMap("d_affine.apply", apply.map)
      case min: d_affine.Min =>
        validateAffineMap("d_affine.min", min.map)
      case ifOp: scf.IfOp => validateIf(ifOp)
      case _              => op.regions.foreach(_.blocks.foreach(_.operations.foreach(validateOp)))

  override def transform(op: Operation): Operation =
    validateOp(op)
    op

package scair.passes.lower_memref_to_llvm

import scair.MLContext
import scair.exceptions.VerifyException
import scair.passes.convert_arith_to_llvm.ConvertArithToLLVM
import scair.passes.finalize_dynamic_memref_to_llvm.FinalizeDynamicMemrefToLLVM
import scair.passes.finalize_refined_dmemref_to_llvm.FinalizeRefinedDMemrefToLLVM
import scair.passes.d_affine_loop_invariant_code_motion.DAffineLoopInvariantCodeMotion
import scair.passes.lower_baseline_control_flow_to_llvm.LowerBaselineControlFlowToLLVM
import scair.passes.lower_refined_control_flow_to_llvm.LowerRefinedControlFlowToLLVM
import scair.passes.normalize_refined_layout_accesses.NormalizeRefinedLayoutAccesses
import scair.passes.refine_dynamic_layout_to_dmemref.RefineDynamicLayoutToDMemref
import scair.passes.dce.DeadCodeElimination
import scair.transformations.ModulePass
import scair.ir.Operation
import scair.verify.Verifier

private def verifyAfterPass(op: Operation, pass: ModulePass): Operation =
  Verifier.verify(op, Verifier.defaultChecks) match
    case scair.utils.Err(errorMsg) =>
      throw VerifyException(
        s"verification failed after pass '${pass.name}':\n$errorMsg"
      )
    case _ => op

private def runPipeline(op: Operation, passes: Seq[ModulePass]): Operation =
  passes.foldLeft(op) { (cur, pass) =>
    val out = pass.transform(cur)
    verifyAfterPass(out, pass)
  }

private def baselineDynamicTail(ctx: MLContext): Seq[ModulePass] =
  Seq(
    LowerBaselineControlFlowToLLVM(ctx),
    ConvertArithToLLVM(ctx),
    FinalizeDynamicMemrefToLLVM(ctx),
  )

private def pointerBasedRefinedTail(ctx: MLContext): Seq[ModulePass] =
  Seq(
    LowerRefinedControlFlowToLLVM(ctx),
    ConvertArithToLLVM(ctx),
    FinalizeRefinedDMemrefToLLVM(ctx),
  )

private def dynamicToRefinedPrefix(ctx: MLContext): Seq[ModulePass] =
  Seq(
    RefineDynamicLayoutToDMemref(ctx),
    NormalizeRefinedLayoutAccesses(ctx),
    DAffineLoopInvariantCodeMotion(ctx),
  )

// Baseline dynamic route.
// Example: `affine.for` + `memref.load`
//   -> LLVM CFG + baseline memref descriptor lowering.
final class LowerDynamicMemrefToLLVMBaseline(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-dynamic-memref-to-llvm-baseline"
  private val passes = baselineDynamicTail(ctx)
  override def transform(op: Operation): Operation =
    runPipeline(op, passes)

// Dynamic memref route through value-dependent d_memref lowering.
// Example: `memref.load` / `memref.store`
//   -> refined layout normalization
//   -> pointer-based LLVM GEP/load/store without descriptors.
final class LowerDynamicMemrefToLLVM(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-dynamic-memref-to-llvm"
  private val passes =
    dynamicToRefinedPrefix(ctx) ++
      pointerBasedRefinedTail(ctx) ++
      Seq(DeadCodeElimination(ctx))
  override def transform(op: Operation): Operation =
    runPipeline(op, passes)

// Direct value-dependent d_memref route.
// Example: `d_memref.load` / `d_memref.store`
//   -> pointer-based LLVM GEP/load/store without descriptors.
final class LowerDMemrefToLLVM(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-dmemref-to-llvm"
  private val passes = pointerBasedRefinedTail(ctx) ++ Seq(DeadCodeElimination(ctx))
  override def transform(op: Operation): Operation =
    runPipeline(op, passes)

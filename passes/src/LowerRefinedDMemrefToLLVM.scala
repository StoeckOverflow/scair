package scair.passes.lower_refined_dmemref_to_llvm

import scair.MLContext
import scair.passes.convert_refined_arith_to_llvm.ConvertRefinedArithToLLVM
import scair.passes.finalize_dynamic_memref_to_llvm.FinalizeDynamicMemrefToLLVM
import scair.passes.finalize_refined_dmemref_to_llvm.FinalizeRefinedDMemrefToLLVM
import scair.passes.hoist_refined_layout_invariants.HoistRefinedLayoutInvariants
import scair.passes.lower_baseline_control_flow_to_llvm.LowerBaselineControlFlowToLLVM
import scair.passes.lower_refined_control_flow_to_llvm.LowerRefinedControlFlowToLLVM
import scair.passes.normalize_refined_layout_accesses.NormalizeRefinedLayoutAccesses
import scair.passes.refine_dynamic_layout_to_dmemref.RefineDynamicLayoutToDMemref
import scair.passes.dce.DeadCodeElimination
import scair.transformations.ModulePass
import scair.ir.Operation

final class LowerRefinedDMemrefToLLVMPipeline(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-refined-dmemref-to-llvm-pipeline"
  private val passes = Seq[ModulePass](
    LowerRefinedControlFlowToLLVM(ctx),
    ConvertRefinedArithToLLVM(ctx),
    FinalizeRefinedDMemrefToLLVM(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

final class LowerDynamicMemrefToLLVMPipeline(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-dynamic-memref-to-llvm-pipeline"
  private val passes = Seq[ModulePass](
    LowerBaselineControlFlowToLLVM(ctx),
    ConvertRefinedArithToLLVM(ctx),
    FinalizeDynamicMemrefToLLVM(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

final class LowerRefinedDMemrefToLLVMBaseline(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-refined-dmemref-to-llvm-baseline"
  private val passes = Seq[ModulePass](
    RefineDynamicLayoutToDMemref(ctx),
    LowerRefinedDMemrefToLLVMPipeline(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

final class LowerRefinedDMemrefToLLVMOptimized(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-refined-dmemref-to-llvm-optimized"
  private val passes = Seq[ModulePass](
    RefineDynamicLayoutToDMemref(ctx),
    NormalizeRefinedLayoutAccesses(ctx),
    HoistRefinedLayoutInvariants(ctx),
    LowerRefinedControlFlowToLLVM(ctx),
    ConvertRefinedArithToLLVM(ctx),
    FinalizeRefinedDMemrefToLLVM(ctx),
    DeadCodeElimination(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

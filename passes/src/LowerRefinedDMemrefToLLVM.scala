package scair.passes.lower_refined_dmemref_to_llvm

import scair.MLContext
import scair.passes.canonicalize_dependent_layouts.CanonicalizeDependentLayouts
import scair.passes.convert_refined_arith_to_llvm.ConvertRefinedArithToLLVM
import scair.passes.expand_baseline_strided_metadata.ExpandBaselineStridedMetadata
import scair.passes.expand_refined_strided_metadata.ExpandRefinedStridedMetadata
import scair.passes.finalize_dynamic_memref_to_llvm.FinalizeDynamicMemrefToLLVM
import scair.passes.finalize_refined_dmemref_to_llvm.FinalizeRefinedDMemrefToLLVM
import scair.passes.hoist_refined_layout_invariants.HoistRefinedLayoutInvariants
import scair.passes.lower_baseline_control_flow_to_llvm.LowerBaselineControlFlowToLLVM
import scair.passes.lower_refined_control_flow_to_llvm.LowerRefinedControlFlowToLLVM
import scair.passes.normalize_refined_layout_accesses.NormalizeRefinedLayoutAccesses
import scair.passes.refine_dynamic_layout_to_dmemref.RefineDynamicLayoutToDMemref
import scair.passes.refine_memref_layout_types.RefineMemrefLayoutTypes
import scair.passes.dce.DeadCodeElimination
import scair.transformations.ModulePass
import scair.ir.Operation

final class NormalizeRefinedDMemref(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "normalize-refined-dmemref"
  private val passes = Seq(
    RefineMemrefLayoutTypes(ctx),
    CanonicalizeDependentLayouts(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

final class LowerRefinedDMemrefToLLVMPipeline(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-refined-dmemref-to-llvm-pipeline"
  private val passes = Seq[ModulePass](
    NormalizeRefinedDMemref(ctx),
    LowerRefinedControlFlowToLLVM(ctx),
    ConvertRefinedArithToLLVM(ctx),
    ExpandRefinedStridedMetadata(ctx),
    FinalizeRefinedDMemrefToLLVM(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

final class LowerRefinedDMemrefToLLVM(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-refined-dmemref-to-llvm"
  private val pipeline = LowerRefinedDMemrefToLLVMPipeline(ctx)
  override def transform(op: Operation): Operation =
    pipeline.transform(op)

final class LowerDynamicMemrefToLLVMPipeline(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-dynamic-memref-to-llvm-pipeline"
  private val passes = Seq[ModulePass](
    LowerBaselineControlFlowToLLVM(ctx),
    ConvertRefinedArithToLLVM(ctx),
    ExpandBaselineStridedMetadata(ctx),
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
    NormalizeRefinedDMemref(ctx),
    LowerRefinedControlFlowToLLVM(ctx),
    ConvertRefinedArithToLLVM(ctx),
    ExpandRefinedStridedMetadata(ctx),
    FinalizeRefinedDMemrefToLLVM(ctx),
    DeadCodeElimination(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

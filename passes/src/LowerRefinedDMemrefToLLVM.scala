package scair.passes.lower_refined_dmemref_to_llvm

import scair.MLContext
import scair.passes.convert_refined_arith_to_llvm.ConvertRefinedArithToLLVM
import scair.passes.finalize_dynamic_memref_to_llvm.FinalizeDynamicMemrefToLLVM
import scair.passes.finalize_refined_dmemref_to_llvm_descriptor.FinalizeRefinedDMemrefDescriptorToLLVM
import scair.passes.finalize_refined_dmemref_to_llvm.FinalizeRefinedDMemrefToLLVM
import scair.passes.hoist_refined_layout_invariants.HoistRefinedLayoutInvariants
import scair.passes.lower_baseline_control_flow_to_llvm.LowerBaselineControlFlowToLLVM
import scair.passes.lower_refined_control_flow_to_llvm.LowerRefinedControlFlowToLLVM
import scair.passes.normalize_refined_layout_accesses.NormalizeRefinedLayoutAccesses
import scair.passes.refine_dynamic_layout_to_dmemref.RefineDynamicLayoutToDMemref
import scair.passes.dce.DeadCodeElimination
import scair.transformations.ModulePass
import scair.ir.Operation

// Core refined lowering pipeline.
// Example: refined `d_memref.load` / `d_memref.store`
//   -> explicit SSA layout arithmetic + pointer-based LLVM GEP/load/store.
final class LowerRefinedDMemrefToLLVMPipeline(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-refined-dmemref-to-llvm-pipeline"
  private val passes = Seq[ModulePass](
    LowerRefinedControlFlowToLLVM(ctx),
    ConvertRefinedArithToLLVM(ctx),
    FinalizeRefinedDMemrefToLLVM(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

// Baseline dynamic route.
// Example: `affine.for` + `memref.load`
//   -> LLVM CFG + baseline memref descriptor lowering.
final class LowerDynamicMemrefToLLVMPipeline(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-dynamic-memref-to-llvm-pipeline"
  private val passes = Seq[ModulePass](
    LowerBaselineControlFlowToLLVM(ctx),
    ConvertRefinedArithToLLVM(ctx),
    FinalizeDynamicMemrefToLLVM(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

// Refined baseline route.
// Example: `memref.reinterpret_cast` + `affine.load`
//   -> `d_memref.reinterpret_cast` + `d_memref.load`
//   -> LLVM lowering without refined access normalization or hoisting.
final class LowerRefinedDMemrefToLLVMBaseline(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "lower-refined-dmemref-to-llvm-baseline"
  private val passes = Seq[ModulePass](
    RefineDynamicLayoutToDMemref(ctx),
    LowerRefinedControlFlowToLLVM(ctx),
    ConvertRefinedArithToLLVM(ctx),
    FinalizeRefinedDMemrefDescriptorToLLVM(ctx),
  )
  override def transform(op: Operation): Operation =
    passes.foldLeft(op)((cur, pass) => pass.transform(cur))

// Refined optimized route.
// Example: `d_memref.load %view[%i, %j]`
//   -> explicit linearized index + flat 1D `d_memref.load`
//   -> pointer-based LLVM with hoisted row-base arithmetic.
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

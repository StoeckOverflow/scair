package scair.passes

import scair.MLContext
import scair.passes.benchmark_constant_folding.BenchmarkConstantFolding
import scair.passes.canonicalization.Canonicalize
import scair.passes.cdt.DummyPass
import scair.passes.cdt.TestInsertionPass
import scair.passes.cdt.TestReplacementPass
import scair.passes.cse.CommonSubexpressionElimination
import scair.passes.convert_arith_to_llvm.ConvertArithToLLVM
import scair.passes.convert_func_to_llvm.ConvertFuncToLLVM
import scair.passes.convert_llvm_export_abi.ConvertLLVMExportABI
import scair.passes.dce.DeadCodeElimination
import scair.passes.d_memref_bounds.DMemrefBoundsCheck
import scair.passes.finalize_dynamic_memref_to_llvm.FinalizeDynamicMemrefToLLVM
import scair.passes.finalize_refined_dmemref_to_llvm.FinalizeRefinedDMemrefToLLVM
import scair.passes.d_affine_loop_invariant_code_motion.DAffineLoopInvariantCodeMotion
import scair.passes.lower_baseline_control_flow_to_llvm.LowerBaselineControlFlowToLLVM
import scair.passes.lower_refined_control_flow_to_llvm.LowerRefinedControlFlowToLLVM
import scair.passes.lower_memref_to_llvm.LowerDMemrefToLLVM
import scair.passes.lower_memref_to_llvm.LowerDynamicMemrefToLLVM
import scair.passes.lower_memref_to_llvm.LowerDynamicMemrefToLLVMBaseline
import scair.passes.normalize_refined_layout_accesses.NormalizeRefinedLayoutAccesses
import scair.passes.refine_dynamic_layout_to_dmemref.RefineDynamicLayoutToDMemref
import scair.passes.reconcile.ReconcileUnrealizedCasts
import scair.passes.dtensor_shape_canonicalize.DTensorShapeCanonicalize
import scair.passes.dtensor_to_dmemref.DTensorToDMemrefShapePreserving
import scair.transformations.ModulePass

//
// ░█████╗░ ██╗░░░░░ ██╗░░░░░
// ██╔══██╗ ██║░░░░░ ██║░░░░░
// ███████║ ██║░░░░░ ██║░░░░░
// ██╔══██║ ██║░░░░░ ██║░░░░░
// ██║░░██║ ███████╗ ███████╗
// ╚═╝░░╚═╝ ╚══════╝ ╚══════╝
//
// ██████╗░ ░█████╗░ ░██████╗ ░██████╗ ███████╗ ░██████╗
// ██╔══██╗ ██╔══██╗ ██╔════╝ ██╔════╝ ██╔════╝ ██╔════╝
// ██████╔╝ ███████║ ╚█████╗░ ╚█████╗░ █████╗░░ ╚█████╗░
// ██╔═══╝░ ██╔══██║ ░╚═══██╗ ░╚═══██╗ ██╔══╝░░ ░╚═══██╗
// ██║░░░░░ ██║░░██║ ██████╔╝ ██████╔╝ ███████╗ ██████╔╝
// ╚═╝░░░░░ ╚═╝░░╚═╝ ╚═════╝░ ╚═════╝░ ╚══════╝ ╚═════╝░
//

val allPasses: Seq[MLContext => ModulePass] =
  Seq(
    BenchmarkConstantFolding(_),
    DTensorShapeCanonicalize(_),
    CommonSubexpressionElimination(_),
    DeadCodeElimination(_),
    RefineDynamicLayoutToDMemref(_),
    NormalizeRefinedLayoutAccesses(_),
    DAffineLoopInvariantCodeMotion(_),
    LowerBaselineControlFlowToLLVM(_),
    LowerRefinedControlFlowToLLVM(_),
    ConvertArithToLLVM(_),
    ConvertFuncToLLVM(_),
    ConvertLLVMExportABI(_),
    FinalizeDynamicMemrefToLLVM(_),
    FinalizeRefinedDMemrefToLLVM(_),
    LowerDynamicMemrefToLLVMBaseline(_),
    LowerDynamicMemrefToLLVM(_),
    LowerDMemrefToLLVM(_),
    DMemrefBoundsCheck(_),
    DTensorToDMemrefShapePreserving(_),
    DummyPass(_),
    ReconcileUnrealizedCasts(_),
    TestInsertionPass(_),
    TestReplacementPass(_),
    Canonicalize(_),
  )

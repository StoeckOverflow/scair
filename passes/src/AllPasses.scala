package scair.passes

import scair.MLContext
import scair.passes.benchmark_constant_folding.BenchmarkConstantFolding
import scair.passes.canonicalization.Canonicalize
import scair.passes.cdt.DummyPass
import scair.passes.cdt.TestInsertionPass
import scair.passes.cdt.TestReplacementPass
import scair.passes.d_affine_to_scf.DAffineToSCF
import scair.passes.cse.CommonSubexpressionElimination
import scair.passes.convert_arith_to_llvm.ConvertArithToLLVM
import scair.passes.d_affine_min_simplify.DAffineMinSimplify
import scair.passes.dce.DeadCodeElimination
import scair.passes.d_linalg_to_d_affine.LowerDLinalgToDAffine
import scair.passes.d_linalg_to_dmemref.BufferizeDLinalgToDMemref
import scair.passes.d_memref_bounds.DMemrefBoundsCheck
import scair.passes.dtensor_matmul_to_tiled_dmemref.DTensorMatmulToTiledDMemref
import scair.passes.finalize_dynamic_memref_to_llvm.FinalizeDynamicMemrefToLLVM
import scair.passes.finalize_refined_dmemref_to_llvm.FinalizeRefinedDMemrefToLLVM
import scair.passes.hoist_refined_layout_invariants.HoistRefinedLayoutInvariants
import scair.passes.hoist_refined_llvm_invariants.HoistRefinedLLVMInvariants
import scair.passes.lower_baseline_control_flow_to_llvm.LowerBaselineControlFlowToLLVM
import scair.passes.lower_refined_control_flow_to_llvm.LowerRefinedControlFlowToLLVM
import scair.passes.lower_memref_to_llvm.LowerDMemrefToLLVM
import scair.passes.lower_memref_to_llvm.LowerDynamicMemrefToLLVM
import scair.passes.lower_memref_to_llvm.LowerDynamicMemrefToLLVMBaseline
import scair.passes.normalize_refined_layout_accesses.NormalizeRefinedLayoutAccesses
import scair.passes.refine_dynamic_layout_to_dmemref.RefineDynamicLayoutToDMemref
import scair.passes.reconcile.ReconcileUnrealizedCasts
import scair.passes.dtensor_to_d_linalg.LowerDTensorToDLinalg
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
    HoistRefinedLayoutInvariants(_),
    HoistRefinedLLVMInvariants(_),
    LowerDTensorToDLinalg(_),
    BufferizeDLinalgToDMemref(_),
    LowerDLinalgToDAffine(_),
    LowerBaselineControlFlowToLLVM(_),
    LowerRefinedControlFlowToLLVM(_),
    ConvertArithToLLVM(_),
    FinalizeDynamicMemrefToLLVM(_),
    FinalizeRefinedDMemrefToLLVM(_),
    LowerDynamicMemrefToLLVMBaseline(_),
    LowerDynamicMemrefToLLVM(_),
    LowerDMemrefToLLVM(_),
    DAffineMinSimplify(_),
    DAffineToSCF(_),
    DMemrefBoundsCheck(_),
    DTensorToDMemrefShapePreserving(_),
    DTensorMatmulToTiledDMemref(_),
    DummyPass(_),
    ReconcileUnrealizedCasts(_),
    TestInsertionPass(_),
    TestReplacementPass(_),
    Canonicalize(_),
  )

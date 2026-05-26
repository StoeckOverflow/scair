package scair.passes

import scair.MLContext
import scair.passes.benchmark_constant_folding.BenchmarkConstantFolding
import scair.passes.canonicalization.Canonicalize
import scair.passes.canonicalize_dtensor_nat_products.CanonicalizeDTensorNatProducts
import scair.passes.cdt.DummyPass
import scair.passes.cdt.TestInsertionPass
import scair.passes.cdt.TestReplacementPass
import scair.passes.convert_arith_to_llvm.ConvertArithToLLVM
import scair.passes.convert_func_to_llvm.ConvertFuncToLLVM
import scair.passes.cse.CommonSubexpressionElimination
import scair.passes.convert_arith_to_llvm.ConvertArithToLLVM
import scair.passes.convert_func_to_llvm.ConvertFuncToLLVM
import scair.passes.convert_llvm_export_abi.ConvertLLVMExportABI
import scair.passes.context_band_tiling.DependentContextBandExactTile
import scair.passes.context_band_tiling.DependentContextBandFactorTileWithTail
import scair.passes.context_band_tiling.DependentContextBandSeparableTile
import scair.passes.context_band_tiling.DependentContextBandTileWithTail
import scair.passes.context_band_tiling.OrdinaryAffineContextBandTileWithTail
import scair.passes.dce.DeadCodeElimination
import scair.passes.d_memref_bounds.DMemrefBoundsCheck
import scair.passes.finalize_dynamic_memref_to_llvm.FinalizeDynamicMemrefToLLVM
import scair.passes.finalize_refined_dmemref_to_llvm.FinalizeRefinedDMemrefToLLVM
import scair.passes.d_affine_to_affine_compatible.DAffineToAffineCompatible
import scair.passes.d_affine_loop_invariant_code_motion.DAffineLoopInvariantCodeMotion
import scair.passes.dependent_natmul_loop_factorization.DependentNatmulLoopFactorization
import scair.passes.dependent_natmul_tiling.DependentExactTile
import scair.passes.dependent_natmul_tiling.DependentProductLoopExactTile
import scair.passes.dependent_natmul_tiling.DependentProductLoopSeparableTile
import scair.passes.dependent_natmul_tiling.DependentTileWithTailControl
import scair.passes.dependent_dim_query_elim.DependentDimQueryElim
import scair.passes.dependent_tail_min_simplify.DependentTailMinSimplify
import scair.passes.dependent_natmul_tiling.OrdinaryAffineProductLoopTileWithTail
import scair.passes.dependent_natmul_tiling.OrdinaryProductTileWithTail
import scair.passes.lower_baseline_control_flow_to_llvm.LowerBaselineControlFlowToLLVM
import scair.passes.lower_cf_assert_to_llvm.LowerCFAssertToLLVM
import scair.passes.lower_refined_control_flow_to_llvm.LowerRefinedControlFlowToLLVM
import scair.passes.lower_memref_to_llvm.LowerDMemrefToLLVM
import scair.passes.lower_memref_to_llvm.LowerDynamicMemrefToLLVM
import scair.passes.lower_memref_to_llvm.LowerDynamicMemrefToLLVMBaseline
import scair.passes.normalize_refined_layout_accesses.NormalizeRefinedLayoutAccesses
import scair.passes.refine_dynamic_layout_to_dmemref.RefineDynamicLayoutToDMemref
import scair.passes.refine_positive_nats_from_asserts.RefinePositiveNatsFromAsserts
import scair.passes.reconcile.ReconcileUnrealizedCasts
import scair.passes.validate_d_affine_dynamic_steps.ValidateDAffineDynamicSteps
import scair.passes.validate_refined_control_flow_lowerable.ValidateRefinedControlFlowLowerable
import scair.passes.dtensor_shape_canonicalize.DTensorShapeCanonicalize
import scair.passes.dtensor_to_dmemref.DTensorToDMemrefShapePreserving
import scair.passes.erase_dtensor_nat_proofs_to_index.EraseDTensorNatProofsToIndex
import scair.transformations.ModulePass
import scair.passes.MonomorphizePass
import scair.passes.LowerTLamToFuncPass
import scair.passes.EraseTLamPass
import scair.passes.BetaReduceTLamPass
import scair.passes.MonomorphizeTlamDeBruijnPass
import scair.passes.LowerTlamDeBruijnToFuncPass
import scair.passes.EraseTlamDeBruijnPass
import scair.passes.BetaReduceTlamDeBruijnPass

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
    CanonicalizeDTensorNatProducts(_),
    BetaReduceTLamPass(_),
    BetaReduceTlamDeBruijnPass(_),
    CommonSubexpressionElimination(_),
    DeadCodeElimination(_),
    RefineDynamicLayoutToDMemref(_),
    NormalizeRefinedLayoutAccesses(_),
    MonomorphizePass(_),
    MonomorphizeTlamDeBruijnPass(_),
    EraseTLamPass(_),
    EraseTlamDeBruijnPass(_),
    LowerTLamToFuncPass(_),
    LowerTlamDeBruijnToFuncPass(_),
    DAffineLoopInvariantCodeMotion(_),
    RefinePositiveNatsFromAsserts(_),
    ValidateDAffineDynamicSteps(_),
    ValidateRefinedControlFlowLowerable(_),
    DependentNatmulLoopFactorization(_),
    DAffineToAffineCompatible(_),
    OrdinaryProductTileWithTail(_),
    OrdinaryAffineProductLoopTileWithTail(_, BigInt(1)),
    OrdinaryAffineContextBandTileWithTail(_, BigInt(1)),
    DependentContextBandExactTile(_),
    DependentContextBandFactorTileWithTail(_),
    DependentContextBandSeparableTile(_),
    DependentContextBandTileWithTail(_, BigInt(1)),
    DependentExactTile(_),
    DependentProductLoopExactTile(_),
    DependentProductLoopSeparableTile(_),
    DependentTileWithTailControl(_),
    DependentDimQueryElim(_),
    DependentTailMinSimplify(_),
    EraseDTensorNatProofsToIndex(_),
    LowerBaselineControlFlowToLLVM(_),
    LowerRefinedControlFlowToLLVM(_),
    LowerCFAssertToLLVM(_),
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

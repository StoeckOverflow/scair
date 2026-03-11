package scair.passes

import scair.MLContext
import scair.passes.benchmark_constant_folding.BenchmarkConstantFolding
import scair.passes.canonicalization.Canonicalize
import scair.passes.cdt.DummyPass
import scair.passes.cdt.TestInsertionPass
import scair.passes.cdt.TestReplacementPass
import scair.passes.d_affine_to_scf.DAffineToSCF
import scair.passes.cse.CommonSubexpressionElimination
import scair.passes.d_affine_min_simplify.DAffineMinSimplify
import scair.passes.dce.DeadCodeElimination
import scair.passes.d_linalg_to_d_affine.LowerDLinalgToDAffine
import scair.passes.d_linalg_to_dmemref.BufferizeDLinalgToDMemref
import scair.passes.d_memref_bounds.DMemrefBoundsCheck
import scair.passes.dtensor_matmul_to_tiled_dmemref.DTensorMatmulToTiledDMemref
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
    LowerDTensorToDLinalg(_),
    BufferizeDLinalgToDMemref(_),
    LowerDLinalgToDAffine(_),
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

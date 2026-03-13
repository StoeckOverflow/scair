package scair.passes.lower_refined_dmemref_to_llvm

import scair.MLContext
import scair.passes.canonicalize_dependent_layouts.CanonicalizeDependentLayouts
import scair.passes.convert_refined_arith_to_llvm.ConvertRefinedArithToLLVM
import scair.passes.expand_refined_strided_metadata.ExpandRefinedStridedMetadata
import scair.passes.finalize_refined_dmemref_to_llvm.FinalizeRefinedDMemrefToLLVM
import scair.passes.lower_refined_control_flow_to_llvm.LowerRefinedControlFlowToLLVM
import scair.passes.refine_memref_layout_types.RefineMemrefLayoutTypes
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
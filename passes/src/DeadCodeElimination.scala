package scair.passes.dce

import scair.MLContext
import scair.dialects.d_tensor.SizeParam
import scair.dialects.tlam.{TLambda as SsaTLambda, VLambda as SsaVLambda}
import scair.dialects.tlam_de_bruijn.{
  TLambda as DbiTLambda,
  VLambda as DbiVLambda,
}
import scair.ir.*
import scair.transformations.*

// Minimal MLIR-like DCE: erase pure ops whose results are unused, including
// type-embedded uses tracked through Value.typeUses.
private def unusedResults(op: Operation): Boolean =
  op.results.forall(r => r.uses.isEmpty && r.typeUses.isEmpty)

private def isLambda(op: Operation): Boolean =
  op match
    case _: SsaTLambda | _: SsaVLambda | _: DbiTLambda | _: DbiVLambda => true
    case _                                                            => false

private val RemoveUnusedOperations = pattern {
  case _: IsTerminator => PatternAction.Abort
  case op if isLambda(op) && unusedResults(op) =>
    PatternAction.Erase
  case op: SizeParam if unusedResults(op) =>
    PatternAction.Erase
  case op: NoMemoryEffect if unusedResults(op) =>
    PatternAction.Erase
  case _: NoMemoryEffect => PatternAction.Abort
}

final class DeadCodeElimination(ctx: MLContext) extends WalkerPass(ctx):
  override val name = "dce"

  override val walker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(RemoveUnusedOperations))
  )

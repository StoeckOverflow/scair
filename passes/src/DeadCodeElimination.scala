package scair.passes.dce

import scair.MLContext
import scair.dialects.dTensor.NatParam
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

// Minimal MLIR-like DCE: erase pure ops whose results are unused, including
// type-embedded uses tracked through Value.typeUses.
private val RemoveUnusedOperations = pattern {
  case _: IsTerminator => PatternAction.Abort
  case op: NatParam
      if op.results.forall(r => r.uses.isEmpty && r.typeUses.isEmpty) =>
    PatternAction.Erase
  case op: NoMemoryEffect
      if op.results.forall(r => r.uses.isEmpty && r.typeUses.isEmpty) =>
    PatternAction.Erase
  case _: NoMemoryEffect => PatternAction.Abort
}

final class DeadCodeElimination(ctx: MLContext) extends WalkerPass(ctx):
  override val name = "dce"

  override val walker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(RemoveUnusedOperations))
  )

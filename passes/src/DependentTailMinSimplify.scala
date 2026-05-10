package scair.passes.dependent_tail_min_simplify

import scair.MLContext
import scair.dialects.affine
import scair.dialects.arith
import scair.dialects.d_affine
import scair.ir.*
import scair.passes.analysis.TailBoundFacts
import scair.transformations.{
  GreedyRewritePatternApplier,
  PatternAction,
  PatternRewriteWalker,
  WalkerPass,
  pattern,
}

private def rewriteClamp(op: Operation): PatternAction | (Seq[Operation], Value[Attribute]) =
  val rewrite = for
    loop <- TailBoundFacts.enclosingDAffineFor(op)
    clamp <- TailBoundFacts.tailClamp(op.results.head)
      .find(TailBoundFacts.canDropClamp(_, loop))
  yield (clamp.replacementOps, clamp.replacementValue)

  rewrite.getOrElse(PatternAction.Abort)

private val EliminateArithDependentTailMin = pattern { case op: arith.MinSI =>
  rewriteClamp(op)
}

private val EliminateDAffineDependentTailMin = pattern { case op: d_affine.Min =>
  rewriteClamp(op)
}

private val EliminateAffineDependentTailMin = pattern { case op: affine.Min =>
  rewriteClamp(op)
}

final class DependentTailMinSimplify(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "dependent-tail-min-simplify"

  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(
        Seq(
          EliminateArithDependentTailMin,
          EliminateDAffineDependentTailMin,
          EliminateAffineDependentTailMin,
        )
      )
    )

package scair.passes.dependent_dim_query_elim

import scair.MLContext
import scair.dialects.{d_tensor as DTensor}
import scair.ir.*
import scair.transformations.{
  GreedyRewritePatternApplier,
  PatternAction,
  PatternRewriteWalker,
  WalkerPass,
  pattern,
}
import scair.utils.*

private val EliminateDTensorDim = pattern { case op: DTensor.Dim =>
  op.selectedDimValue match
    case OK(selected) =>
      DTensor.DTensorTypeUtil.resolveIndexValue(selected) match
        case OK(baseIndex) => (Seq.empty[Operation], baseIndex)
        case _             => PatternAction.Abort
    case _ => PatternAction.Abort
}

final class DependentDimQueryElim(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "dependent-dim-query-elim"

  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(
        Seq(
          EliminateDTensorDim,
        )
      )
    )

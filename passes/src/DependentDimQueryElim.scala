package scair.passes.dependent_dim_query_elim

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor
import scair.ir.*
import scair.transformations.{
  GreedyRewritePatternApplier,
  PatternAction,
  PatternRewriteWalker,
  WalkerPass,
  pattern,
}
import scair.utils.*

private val EliminateDTensorDim = pattern { case op: dTensor.Dim =>
  op.selectedDimValue match
    case OK(selected) =>
      dTensor.dTensorTypeUtil.resolveNatValue(selected) match
        case OK(baseNat) => (Seq.empty[Operation], baseNat)
        case _           => PatternAction.Abort
    case _ => PatternAction.Abort
}

private val ShapeToIndexNatConst = pattern { case op: dTensor.ShapeToIndex =>
  dTensor.dTensorTypeUtil.resolveNatValue(op.nat) match
    case OK(nat) =>
      nat.owner match
        case Some(dTensor.NatConst(value, _)) =>
          arith.Constant(
            IntegerAttr(IntData(value.value.value), IndexType()),
            Result(IndexType()),
          )
        case _ => PatternAction.Abort
    case _ => PatternAction.Abort
}

final class DependentDimQueryElim(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "dependent-dim-query-elim"

  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(
        Seq(
          EliminateDTensorDim,
          ShapeToIndexNatConst,
        )
      )
    )

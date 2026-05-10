package scair.passes.dtensor_shape_canonicalize

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.ir.*
import scair.transformations.{GreedyRewritePatternApplier, PatternRewriteWalker, WalkerPass, PatternAction, pattern}

private def constValue(
    v: Value[dTensorNatLikeType]
): Option[(BigInt, IntegerType | IndexType)] =
  v.owner match
    case Some(NatConst(IntegerAttr(IntData(k), typ), _)) => Some((k, typ))
    case _                                               => None

private def mkNatConst(
    k: BigInt,
    typ: IntegerType | IndexType,
    resType: dTensorNatLikeType,
): NatConst =
  NatConst(IntegerAttr(IntData(k), typ), Result(resType))

private val NatAddFold = pattern { case NatAdd(lhs, rhs, res) =>
  (constValue(lhs), constValue(rhs)) match
    case (Some((0, _)), _)              => (Seq(), Seq(rhs))
    case (_, Some((0, _)))              => (Seq(), Seq(lhs))
    case (Some((a, aty)), Some((b, _))) =>
      mkNatConst(a + b, aty, res.typ)
    case _ => PatternAction.Abort
}

private val NatMulFold = pattern { case NatMul(lhs, rhs, res) =>
  (constValue(lhs), constValue(rhs)) match
    case (Some((0, _)), _)              => (Seq(), Seq(lhs))
    case (_, Some((0, _)))              => (Seq(), Seq(rhs))
    case (Some((1, _)), _)              => (Seq(), Seq(rhs))
    case (_, Some((1, _)))              => (Seq(), Seq(lhs))
    case (Some((a, aty)), Some((b, _))) =>
      mkNatConst(a * b, aty, res.typ)
    case _ => PatternAction.Abort
}

final class DTensorShapeCanonicalize(ctx: MLContext) extends WalkerPass(ctx):
  override val name = "tensor-shape-canonicalize"

  override val walker = PatternRewriteWalker(
    GreedyRewritePatternApplier(
      Seq(NatAddFold, NatMulFold)
    )
  )

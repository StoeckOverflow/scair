package scair.passes.erase_d_tensor_nat_proofs_to_index

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.{d_tensor as DTensor}
import scair.dialects.d_affine
import scair.ir.*
import scair.transformations.{
  GreedyRewritePatternApplier,
  ModulePass,
  PatternRewriteWalker,
  pattern,
}

private def asIndex(v: Value[Attribute]): Operand[arith.AnyIntegerType] =
  v.asInstanceOf[Operand[arith.AnyIntegerType]]

private val IndexToNatErase = pattern { case op: DTensor.IndexToNat =>
  (Seq.empty[Operation], op.index)
}

private val NatRefinePositiveErase = pattern { case op: DTensor.NatRefinePositive =>
  (Seq.empty[Operation], op.nat)
}

private val NatConstToIndex = pattern { case op: DTensor.NatConst =>
  arith.Constant(
    IntegerAttr(IntData(op.value.value.value), IndexType()),
    Result(IndexType()),
  )
}

private val NatMulToIndex = pattern { case op: DTensor.NatMul =>
  arith.MulI(
    asIndex(op.lhs),
    asIndex(op.rhs),
    Result(IndexType()),
  )
}

private val NatAddToIndex = pattern { case op: DTensor.NatAdd =>
  arith.AddI(
    asIndex(op.lhs),
    asIndex(op.rhs),
    Result(IndexType()),
  )
}

private val ShapeToIndexErase = pattern { case op: DTensor.ShapeToIndex =>
  (Seq.empty[Operation], op.nat)
}

private def proofConsumerLeft(op: Operation): Option[String] =
  var found = Option.empty[String]
  def visit(cur: Operation): Unit =
    if found.nonEmpty then ()
    else
      cur match
        case _: d_affine.For   => found = Some("d_affine.for")
        case _: d_affine.Apply => found = Some("d_affine.apply")
        case _: d_affine.Min   => found = Some("d_affine.min")
        case _ =>
          cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))
  visit(op)
  found

final class EraseDTensorNatProofsToIndex(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "erase-d-tensor-nat-proofs-to-index"

  private val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(
        Seq(
          IndexToNatErase,
          NatRefinePositiveErase,
          NatConstToIndex,
          NatAddToIndex,
          NatMulToIndex,
          ShapeToIndexErase,
        )
      )
    )

  override def transform(op: Operation): Operation =
    proofConsumerLeft(op).foreach { consumer =>
      throw new Exception(
        s"erase-d-tensor-nat-proofs-to-index cannot run while $consumer remains. " +
          "Run proof-consuming passes first, then d-affine-to-affine-compatible or lower-refined-control-flow-to-llvm before erasing nat proofs."
      )
    }
    walker.rewrite(op)
    op

package scair.passes.erase_d_tensor_size_witnesses_to_index

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

private def asIntegerLike(v: Value[Attribute]): Operand[arith.AnyIntegerType] =
  v.asInstanceOf[Operand[arith.AnyIntegerType]]

private val SizeImportErase = pattern { case op: DTensor.SizeImport =>
  (Seq.empty[Operation], op.index)
}

private val SizePositiveProofErase = pattern { case op: DTensor.SizePositiveProof =>
  (Seq.empty[Operation], op.proof)
}

private val SizeRefinePositiveErase = pattern { case op: DTensor.SizeRefinePositive =>
  (Seq.empty[Operation], op.size)
}

private val SizeConstantToIndex = pattern { case op: DTensor.SizeConstant =>
  arith.Constant(
    IntegerAttr(IntData(op.value.value.value), IndexType()),
    Result(IndexType()),
  )
}

private val SizeMulToIndex = pattern { case op: DTensor.SizeMul =>
  arith.MulI(
    asIntegerLike(op.lhs),
    asIntegerLike(op.rhs),
    Result(IndexType()),
  )
}

private val SizeAddToIndex = pattern { case op: DTensor.SizeAdd =>
  arith.AddI(
    asIntegerLike(op.lhs),
    asIntegerLike(op.rhs),
    Result(IndexType()),
  )
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

final class EraseDTensorSizeWitnessesToIndex(ctx: MLContext) extends ModulePass(ctx):
  override val name: String = "erase-d-tensor-size-witnesses-to-index"

  private val walker: PatternRewriteWalker =
    PatternRewriteWalker(
      GreedyRewritePatternApplier(
        Seq(
          SizeImportErase,
          SizeRefinePositiveErase,
          SizePositiveProofErase,
          SizeConstantToIndex,
          SizeAddToIndex,
          SizeMulToIndex,
        )
      )
    )

  override def transform(op: Operation): Operation =
    proofConsumerLeft(op).foreach { consumer =>
      throw new Exception(
        s"erase-d-tensor-size-witnesses-to-index cannot run while $consumer remains. " +
          "Run proof-consuming passes first, then d-affine-to-affine-compatible or lower-refined-control-flow-to-llvm before erasing size witness proofs."
      )
    }
    walker.rewrite(op)
    op

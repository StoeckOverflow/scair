package scair.passes.d_linalg_to_dmemref

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.dialects.d_linalg
import scair.ir.*
import scair.passes.dtensor_to_dmemref.DTensorDMemrefConversion
import scair.transformations.*
import scair.transformations.patterns.*

private def castBackToTensor(
    memref: Value[Attribute],
    tensorType: dTensorTensorType,
): UnrealizedConversionCastOp =
  UnrealizedConversionCastOp(
    inputs = Seq(memref),
    outputs = Seq(Result(tensorType)),
  )

private val BufferizeFill = pattern {
  case op @ d_linalg.Fill(value, out, results) =>
    out.typ match
      case tensorTy: dTensorTensorType if results.size == 1 =>
        val memTy = DTensorDMemrefConversion.tensorToMemrefType(tensorTy)
        val (prefix, outMem) = DTensorDMemrefConversion.toMemrefValue(out, memTy)
        val lowered = d_linalg.Fill(
          value,
          outMem.asInstanceOf[Operand[Attribute]],
          Seq.empty,
        )
        val castBack = castBackToTensor(outMem, tensorTy)
        (prefix ++ Seq(lowered, castBack), castBack.outputs)
      case _ => PatternAction.Abort
}

private val BufferizeMatmul = pattern {
  case op @ d_linalg.Matmul(lhs, rhs, out, results) =>
    out.typ match
      case tensorTy: dTensorTensorType if results.size == 1 =>
        val lhsMemTy = DTensorDMemrefConversion.tensorToMemrefType(
          lhs.typ.asInstanceOf[dTensorTensorType]
        )
        val rhsMemTy = DTensorDMemrefConversion.tensorToMemrefType(
          rhs.typ.asInstanceOf[dTensorTensorType]
        )
        val outMemTy = DTensorDMemrefConversion.tensorToMemrefType(tensorTy)
        val (lhsPrefix, lhsMem) = DTensorDMemrefConversion.toMemrefValue(lhs, lhsMemTy)
        val (rhsPrefix, rhsMem) = DTensorDMemrefConversion.toMemrefValue(rhs, rhsMemTy)
        val (outPrefix, outMem) = DTensorDMemrefConversion.toMemrefValue(out, outMemTy)
        val lowered = d_linalg.Matmul(
          lhsMem.asInstanceOf[Operand[Attribute]],
          rhsMem.asInstanceOf[Operand[Attribute]],
          outMem.asInstanceOf[Operand[Attribute]],
          Seq.empty,
        )
        val castBack = castBackToTensor(outMem, tensorTy)
        (lhsPrefix ++ rhsPrefix ++ outPrefix ++ Seq(lowered, castBack), castBack.outputs)
      case _ => PatternAction.Abort
}

/**
 * Bufferizes tensor-result `d_linalg` ops into `d_memref` form while preserving a
 * tensor-facing result through an unrealized cast.
 *
 * This pass converts dtensor operands/results to `d_memref` values using the
 * dtensor-to-dmemref bridge, rewrites the operation to its buffer form with no SSA
 * result, and then casts the output buffer back to the original tensor type.
 *
 * Rewrite shapes:
 * `<d_linalg.fill %value, %out_tensor -> %res_tensor>`
 * `->`
 * `<tensor-to-memref bridge + d_linalg.fill %value, %out_memref + unrealized_conversion_cast %out_memref to tensor>`
 *
 * `<d_linalg.matmul %lhs_tensor, %rhs_tensor, %out_tensor -> %res_tensor>`
 * `->`
 * `<tensor-to-memref bridges + d_linalg.matmul %lhs_memref, %rhs_memref, %out_memref + unrealized_conversion_cast %out_memref to tensor>`
 */
final class BufferizeDLinalgToDMemref(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "bufferize-d-linalg-to-dmemref"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(BufferizeFill, BufferizeMatmul))
  )

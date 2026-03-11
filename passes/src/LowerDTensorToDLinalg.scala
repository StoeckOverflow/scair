package scair.passes.dtensor_to_d_linalg

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.dialects.d_linalg
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

private def zeroOf(typ: TypeAttribute): Option[arith.Constant | NatConst] =
  typ match
    case i: IntegerType =>
      Some(arith.Constant(IntegerAttr(IntData(0), i), Result(i)))
    case f: FloatType   =>
      Some(arith.Constant(FloatAttr(FloatData(0.0), f), Result(f)))
    case _              => None

private val LowerFill = pattern {
  case Fill(v, res) =>
    val init = Empty(Result(res.typ))
    val fill = d_linalg.Fill(v, init.res.asInstanceOf[Operand[Attribute]], Seq(Result(res.typ)))
    (Seq(init, fill), fill.results)
}

private val LowerMatmul = pattern {
  case Matmul(lhs, rhs, res) =>
    zeroOf(res.typ.elem) match
      case Some(zero: arith.Constant) =>
        val init = Empty(Result(res.typ))
        val fill =
          d_linalg.Fill(
            zero.result.asInstanceOf[Operand[TypeAttribute]],
            init.res.asInstanceOf[Operand[Attribute]],
            Seq(Result(res.typ)),
          )
        val matmul = d_linalg.Matmul(
          lhs.asInstanceOf[Operand[Attribute]],
          rhs.asInstanceOf[Operand[Attribute]],
          fill.results.head.asInstanceOf[Operand[Attribute]],
          Seq(Result(res.typ)),
        )
        (Seq(zero, init, fill, matmul), matmul.results)
      case _ =>
        PatternAction.Abort
}

/**
 * This pass rewrites `dtensor.fill` and `dtensor.matmul` into `d_linalg`
 * operations by materializing explicit destination tensors.
 *
 * Rewrite shapes:
 * `<dtensor.fill %value -> !dtensor.tensor<...>>`
 * `->`
 * `<dtensor.empty + d_linalg.fill>`
 *
 * `<dtensor.matmul %lhs, %rhs -> !dtensor.tensor<...>>`
 * `->`
 * `<zero constant + dtensor.empty + d_linalg.fill + d_linalg.matmul>`
 */
final class LowerDTensorToDLinalg(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "lower-dtensor-to-d-linalg"

  override val walker: PatternRewriteWalker = PatternRewriteWalker(
    GreedyRewritePatternApplier(Seq(LowerFill, LowerMatmul))
  )

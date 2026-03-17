package scair.passes.convert_refined_arith_to_llvm

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.func
import scair.dialects.llvm
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

import scala.collection.mutable

private def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

private def asFloat(v: Value[Attribute]): Operand[FloatType] =
  v.asInstanceOf[Operand[FloatType]]

private final class Builder(val funcOp: func.Func):
  val blockMap = mutable.Map.empty[Block, Block]
  val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]

  private def remap(v: Value[Attribute]): Value[Attribute] =
    valueMap.getOrElse(v, v)

  private def lowerConstant(op: arith.Constant, block: Block): Operation =
    val lowered = llvm.Constant(op.value, Result(op.result.typ))
    valueMap(op.result) = lowered.res
    lowered

  private def lowerOp(op: Operation): Seq[Operation] =
    op match
      case c: arith.Constant =>
        Seq(llvm.Constant(c.value, Result(c.result.typ)))
      case add: arith.AddI =>
        val lowered = llvm.Add(asIndex(remap(add.lhs)), asIndex(remap(add.rhs)), Result(add.result.typ))
        valueMap(add.result) = lowered.res
        Seq(lowered)
      case mul: arith.MulI =>
        val lowered = llvm.Mul(asIndex(remap(mul.lhs)), asIndex(remap(mul.rhs)), Result(mul.result.typ))
        valueMap(mul.result) = lowered.res
        Seq(lowered)
      case add: arith.AddF =>
        val lowered = llvm.FAdd(asFloat(remap(add.lhs)), asFloat(remap(add.rhs)), Result(add.result.typ))
        valueMap(add.result) = lowered.res
        Seq(lowered)
      case other =>
        val copied = other.deepCopy(using blockMap, valueMap)
        valueMap.addAll(other.results.zip(copied.results))
        Seq(copied)

  def lower(): func.Func =
    val newBlocks = funcOp.body.blocks.map { oldBlock =>
      val nb = Block(oldBlock.arguments.map(_.typ), Seq.empty)
      blockMap(oldBlock) = nb
      valueMap.addAll(oldBlock.arguments.zip(nb.arguments))
      nb
    }
    funcOp.body.blocks.zip(newBlocks).foreach { case (oldBlock, newBlock) =>
      val constants = oldBlock.operations.collect { case c: arith.Constant => c }.toSeq
      constants.foreach { c =>
        val lowered = lowerConstant(c, newBlock)
        newBlock.addOp(lowered)
      }
      oldBlock.operations.foreach {
        case _: arith.Constant => ()
        case other             => newBlock.addOps(lowerOp(other))
      }
    }
    func.Func(funcOp.sym_name, funcOp.function_type, funcOp.sym_visibility, Region(newBlocks))

private val LowerFunc = pattern {
  case op: func.Func if op.body.blocks.exists(_.operations.exists {
        case _: arith.Constant | _: arith.AddI | _: arith.MulI | _: arith.AddF => true
        case _                                                                  => false
      }) =>
    Builder(op).lower()
}

final class ConvertRefinedArithToLLVM(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "convert-refined-arith-to-llvm"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))

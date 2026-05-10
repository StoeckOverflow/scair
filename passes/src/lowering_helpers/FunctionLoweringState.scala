package scair.passes.lowering_helpers

import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.func
import scair.dialects.llvm
import scair.ir.*

import scala.collection.mutable

final class FunctionLoweringState(val funcOp: func.Func):
  val blockMap = mutable.Map.empty[Block, Block]
  val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]

  def remap(v: Value[Attribute]): Value[Attribute] =
    valueMap.getOrElse(v, v)

  def makeClonedBlocks(): Seq[Block] =
    funcOp.body.blocks.map { oldBlock =>
      val nb = Block(oldBlock.arguments.map(_.typ), Seq.empty)
      blockMap(oldBlock) = nb
      valueMap.addAll(oldBlock.arguments.zip(nb.arguments))
      nb
    }

  def deepCopyOp(op: Operation): Operation =
    op match
      case cmp: arith.CmpI =>
        arith.CmpI(
          remap(cmp.lhs).asInstanceOf[Operand[arith.AnyIntegerType]],
          remap(cmp.rhs).asInstanceOf[Operand[arith.AnyIntegerType]],
          Result(I1),
          cmp.predicate,
        )
      case cmp: arith.CmpF =>
        arith.CmpF(
          remap(cmp.lhs).asInstanceOf[Operand[FloatType]],
          remap(cmp.rhs).asInstanceOf[Operand[FloatType]],
          Result(I1),
          cmp.predicate,
          cmp.fastmath,
        )
      case cmp: llvm.ICmp =>
        llvm.ICmp(
          remap(cmp.lhs).asInstanceOf[Operand[IntegerType | IndexType]],
          remap(cmp.rhs).asInstanceOf[Operand[IntegerType | IndexType]],
          Result(cmp.res.typ),
          cmp.predicate,
        )
      case _ =>
        op.deepCopy(using blockMap, valueMap)

package scair.passes.lowering_helpers

import scair.dialects.func
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
    op.deepCopy(using blockMap, valueMap)

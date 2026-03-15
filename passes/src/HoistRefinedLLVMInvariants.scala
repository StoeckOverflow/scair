package scair.passes.hoist_refined_llvm_invariants

import scair.MLContext
import scair.dialects.builtin.*
import scair.dialects.func
import scair.dialects.llvm
import scair.ir.*
import scair.transformations.*
import scair.transformations.patterns.*

import scala.collection.mutable

private def isBaseProjection(position: DenseArrayAttr): Boolean =
  position.length == 1 && position.head == IntegerAttr(IntData(1), I32)

private def ownerBlock(v: Value[Attribute]): Option[Block] =
  v.owner.flatMap {
    case block: Block => Some(block)
    case op: Operation => op.containerBlock
  }

private final class Builder(val funcOp: func.Func):
  private val oldBlocks = funcOp.body.blocks.toSeq
  private val newBlocks = oldBlocks.map(b => Block(b.arguments.map(_.typ), Seq.empty))
  private val blockMap = mutable.Map.from(oldBlocks.zip(newBlocks))
  private val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]
  valueMap.addAll(oldBlocks.flatMap(_.arguments).zip(newBlocks.flatMap(_.arguments)))

  private val hoistedBase = mutable.Map.empty[(Value[Attribute], Attribute), Value[Attribute]]

  private def remap(v: Value[Attribute]): Value[Attribute] =
    valueMap.getOrElse(v, v)

  private def emit(block: Block, op: Operation): Unit =
    block.addOp(op)

  private def hoistBaseProjection(
      container: Value[Attribute],
      position: DenseArrayAttr,
      resTy: Attribute,
  ): Value[Attribute] =
    val entry = newBlocks.head
    val key = (container, resTy)
    hoistedBase.getOrElseUpdate(
      key, {
        val ex = llvm.ExtractValue(
          remap(container).asInstanceOf[Operand[Attribute]],
          position,
          Result(resTy),
        )
        entry.operations.lastOption match
          case Some(term: IsTerminator) => entry.insertOpBefore(term.asInstanceOf[Operation], ex)
          case _                        => entry.addOp(ex)
        ex.res
      }
    )

  def lower(): func.Func =
    oldBlocks.zip(newBlocks).foreach { case (oldBlock, newBlock) =>
      oldBlock.operations.foreach {
        case op: llvm.ExtractValue
            if (oldBlock ne oldBlocks.head) &&
              isBaseProjection(op.position) &&
              ownerBlock(remap(op.container)).contains(newBlocks.head) =>
          valueMap(op.res) = hoistBaseProjection(op.container, op.position, op.res.typ)
        case other =>
          val copied = other.deepCopy(using blockMap, valueMap)
          emit(newBlock, copied)
          valueMap.addAll(other.results.zip(copied.results))
      }
    }
    func.Func(funcOp.sym_name, funcOp.function_type, funcOp.sym_visibility, Region(newBlocks))

private val HoistFunc = pattern {
  case op: func.Func if op.body.blocks.drop(1).exists(_.operations.exists {
        case ex: llvm.ExtractValue => isBaseProjection(ex.position)
        case _                     => false
      }) =>
    Builder(op).lower()
}

final class HoistRefinedLLVMInvariants(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "hoist-refined-llvm-invariants"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(HoistFunc)))

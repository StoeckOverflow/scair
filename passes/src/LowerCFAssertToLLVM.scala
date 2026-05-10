package scair.passes.lower_cf_assert_to_llvm

import scair.MLContext
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.cf
import scair.dialects.func
import scair.dialects.llvm
import scair.ir.*
import scair.transformations.{GreedyRewritePatternApplier, PatternRewriteWalker, WalkerPass, pattern}

import scala.collection.mutable

private def containsAssert(op: Operation): Boolean =
  var found = false
  def visit(cur: Operation): Unit =
    if !found then
      cur match
        case _: cf.Assert => found = true
        case _            => cur.regions.foreach(_.blocks.foreach(_.operations.foreach(visit)))
  visit(op)
  found

private final class FuncAssertLowerer(funcOp: func.Func):
  private val blockMap = mutable.Map.empty[Block, Block]
  private val valueMap = mutable.Map.empty[Value[Attribute], Value[Attribute]]
  private val newBlocks = mutable.ArrayBuffer.empty[Block]

  private def remap(v: Value[Attribute]): Value[Attribute] =
    valueMap.getOrElse(v, v)

  private def operand(v: Value[Attribute]): Operand[Attribute] =
    remap(v).asInstanceOf[Operand[Attribute]]

  private def i1Operand(v: Value[Attribute]): Operand[IntegerType] =
    remap(v).asInstanceOf[Operand[IntegerType]]

  private def lowerAssert(assertOp: cf.Assert, current: Block): Block =
    val passBlock = Block(Seq.empty, Seq.empty)
    val failBlock = Block(Seq.empty, Seq.empty)
    newBlocks += failBlock
    newBlocks += passBlock

    current.addOp(
      llvm.CondBr(
        i1Operand(assertOp.arg),
        Seq.empty,
        Seq.empty,
        passBlock,
        failBlock,
      )
    )
    failBlock.addOp(
      llvm.Call(
        SymbolRefAttr(StringData("abort")),
        Seq.empty,
        Seq.empty,
      )
    )
    failBlock.addOp(llvm.Unreachable())
    passBlock

  private def lowerBlock(oldBlock: Block, initialBlock: Block): Unit =
    var current = initialBlock
    oldBlock.operations.foreach {
      case assertOp: cf.Assert =>
        current = lowerAssert(assertOp, current)
      case cmp: arith.CmpI =>
        val copied = arith.CmpI(
          remap(cmp.lhs).asInstanceOf[Operand[arith.AnyIntegerType]],
          remap(cmp.rhs).asInstanceOf[Operand[arith.AnyIntegerType]],
          Result(I1),
          cmp.predicate,
        )
        current.addOp(copied)
        valueMap(cmp.result) = copied.result
      case cmp: arith.CmpF =>
        val copied = arith.CmpF(
          remap(cmp.lhs).asInstanceOf[Operand[FloatType]],
          remap(cmp.rhs).asInstanceOf[Operand[FloatType]],
          Result(I1),
          cmp.predicate,
          cmp.fastmath,
        )
        current.addOp(copied)
        valueMap(cmp.result) = copied.result
      case cmp: llvm.ICmp =>
        val copied = llvm.ICmp(
          remap(cmp.lhs).asInstanceOf[Operand[IntegerType | IndexType]],
          remap(cmp.rhs).asInstanceOf[Operand[IntegerType | IndexType]],
          Result(cmp.res.typ),
          cmp.predicate,
        )
        current.addOp(copied)
        valueMap(cmp.res) = copied.res
      case other =>
        val copied = other.deepCopy(using blockMap, valueMap)
        current.addOp(copied)
        valueMap.addAll(other.results.zip(copied.results))
    }

  def lower(): func.Func =
    funcOp.body.blocks.foreach { oldBlock =>
      val newBlock = Block(oldBlock.arguments.map(_.typ), Seq.empty)
      blockMap(oldBlock) = newBlock
      valueMap.addAll(oldBlock.arguments.zip(newBlock.arguments))
    }
    funcOp.body.blocks.foreach { oldBlock =>
      val initialBlock = blockMap(oldBlock)
      newBlocks += initialBlock
      lowerBlock(oldBlock, initialBlock)
    }

    val lowered = func.Func(
      funcOp.sym_name,
      funcOp.function_type,
      funcOp.sym_visibility,
      Region(newBlocks.toSeq),
    )
    lowered.attributes.addAll(funcOp.attributes)
    lowered

private val LowerFunc = pattern {
  case op: func.Func if containsAssert(op) =>
    FuncAssertLowerer(op).lower()
}

final class LowerCFAssertToLLVM(ctx: MLContext) extends WalkerPass(ctx):
  override val name: String = "lower-cf-assert-to-llvm"
  override val walker: PatternRewriteWalker =
    PatternRewriteWalker(GreedyRewritePatternApplier(Seq(LowerFunc)))

package scair.passes.control_flow_helpers

import scair.dialects.affine.*
import scair.dialects.builtin.*
import scair.dialects.llvm
import scair.ir.*

import scala.collection.mutable

def asIndex(v: Value[Attribute]): Operand[IndexType] =
  v.asInstanceOf[Operand[IndexType]]

def asI1(v: Value[Attribute]): Operand[IntegerType] =
  v.asInstanceOf[Operand[IntegerType]]

def idxAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), IndexType())

def overflowNSWNuw: ArrayAttribute[StringData] =
  ArrayAttribute(Seq(StringData("nsw"), StringData("nuw")))

def identityOrConstBound(
    operands: Seq[Value[Attribute]],
    map: AffineMapAttr,
): Option[Either[BigInt, Value[Attribute]]] =
  if map.affineMap.affineExprs.size != 1 then None
  else
    val dims = map.affineMap.dimensions
    map.affineMap.affineExprs.head match
      case AffineConstantExpr(v) => Some(Left(v))
      case AffineDimExpr(name) =>
        val idx = dims.indexOf(name)
        if idx < 0 || idx >= operands.size then None else Some(Right(operands(idx)))
      case _ => None

final class LoopCFGBuilder(val blocks: mutable.ArrayBuffer[Block]):
  def appendBlock(block: Block): Unit =
    blocks += block

  def emitIndexConstant(block: Block, v: BigInt): Value[Attribute] =
    val c = llvm.Constant(idxAttr(v), Result(IndexType()))
    block.addOp(c)
    c.res

  def emitICmpSlt(
      block: Block,
      lhs: Value[Attribute],
      rhs: Value[Attribute],
  ): Value[Attribute] =
    val cmp = llvm.ICmp(asIndex(lhs), asIndex(rhs), StringData("slt"), Result(I1))
    block.addOp(cmp)
    cmp.res

  def emitAdd(
      block: Block,
      lhs: Value[Attribute],
      rhs: Value[Attribute],
  ): Value[Attribute] =
    val add = llvm.Add(
      asIndex(lhs),
      asIndex(rhs),
      Result(IndexType()),
      Some(overflowNSWNuw),
    )
    block.addOp(add)
    add.res

  def emitBr(
      block: Block,
      operands: Seq[Value[Attribute]],
      dest: Block,
  ): Unit =
    block.addOp(
      llvm.Br(
        operands.map(_.asInstanceOf[Operand[Attribute]]),
        dest,
      )
    )

  def emitCondBr(
      block: Block,
      cond: Value[Attribute],
      trueArgs: Seq[Value[Attribute]],
      falseArgs: Seq[Value[Attribute]],
      trueDest: Block,
      falseDest: Block,
  ): Unit =
    block.addOp(
      llvm.CondBr(
        asI1(cond),
        trueArgs.map(_.asInstanceOf[Operand[Attribute]]),
        falseArgs.map(_.asInstanceOf[Operand[Attribute]]),
        trueDest,
        falseDest,
      )
    )

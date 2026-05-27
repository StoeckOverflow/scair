package scair.passes.control_flow_helpers

import scair.dialects.builtin.*
import scair.dialects.llvm
import scair.ir.*

import scala.collection.mutable

val llvmIndexType: IntegerType = I64

def asLLVMIndex(v: Value[Attribute]): Operand[IntegerType | IndexType] =
  v.asInstanceOf[Operand[IntegerType | IndexType]]

def asI1(v: Value[Attribute]): Operand[IntegerType] =
  v.asInstanceOf[Operand[IntegerType]]

def llvmIndexAttr(v: BigInt): IntegerAttr =
  IntegerAttr(IntData(v), llvmIndexType)

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

def explainUnsupportedAffineExpr(expr: AffineExpr): Option[String] =
  expr match
    case _: AffineConstantExpr | _: AffineDimExpr | _: AffineSymExpr => None
    case AffineBinaryOpExpr(AffineBinaryOp.Add | AffineBinaryOp.Minus, lhs, rhs) =>
      explainUnsupportedAffineExpr(lhs).orElse(explainUnsupportedAffineExpr(rhs))
    case AffineBinaryOpExpr(AffineBinaryOp.Multiply, lhs, rhs) =>
      (lhs, rhs) match
        case (_: AffineConstantExpr, _) => explainUnsupportedAffineExpr(rhs)
        case (_, _: AffineConstantExpr) => explainUnsupportedAffineExpr(lhs)
        case _                          => Some("multiplication is only supported when one operand is a constant")
    case AffineBinaryOpExpr(AffineBinaryOp.CeilDiv | AffineBinaryOp.FloorDiv | AffineBinaryOp.Mod, _, _) =>
      Some("ceildiv, floordiv, and mod affine expressions are not supported by refined CFG lowering")

def explainUnsupportedAffineMap(map: AffineMapAttr): Option[String] =
  if map.affineMap.affineExprs.size != 1 then
    Some(s"expected single-result affine map, got ${map.affineMap.affineExprs.size} results")
  else explainUnsupportedAffineExpr(map.affineMap.affineExprs.head)

final class LoopCFGBuilder(val blocks: mutable.ArrayBuffer[Block]):
  def appendBlock(block: Block): Unit =
    blocks += block

  def emitIndexConstant(block: Block, v: BigInt): Value[Attribute] =
    val c = llvm.Constant(llvmIndexAttr(v), Result(llvmIndexType))
    block.addOp(c)
    c.res

  def emitICmpSlt(
      block: Block,
      lhs: Value[Attribute],
      rhs: Value[Attribute],
  ): Value[Attribute] =
    val cmp = llvm.ICmp(
      asLLVMIndex(lhs),
      asLLVMIndex(rhs),
      Result(I1),
      llvm.ICmpPredicate.slt,
    )
    block.addOp(cmp)
    cmp.res

  def emitICmp(
      block: Block,
      lhs: Value[Attribute],
      rhs: Value[Attribute],
      predicate: llvm.ICmpPredicate,
  ): Value[Attribute] =
    val cmp = llvm.ICmp(
      asLLVMIndex(lhs),
      asLLVMIndex(rhs),
      Result(I1),
      predicate,
    )
    block.addOp(cmp)
    cmp.res

  def emitAdd(
      block: Block,
      lhs: Value[Attribute],
      rhs: Value[Attribute],
  ): Value[Attribute] =
    val add = llvm.Add(
      asLLVMIndex(lhs),
      asLLVMIndex(rhs),
      Result(llvmIndexType),
    )
    block.addOp(add)
    add.res

  def emitMul(
      block: Block,
      lhs: Value[Attribute],
      rhs: Value[Attribute],
  ): Value[Attribute] =
    val mul = llvm.Mul(
      asLLVMIndex(lhs),
      asLLVMIndex(rhs),
      Result(llvmIndexType),
    )
    block.addOp(mul)
    mul.res

  def emitNeg(block: Block, value: Value[Attribute]): Value[Attribute] =
    val minusOne = emitIndexConstant(block, -1)
    emitMul(block, value, minusOne)

  def emitSub(
      block: Block,
      lhs: Value[Attribute],
      rhs: Value[Attribute],
  ): Value[Attribute] =
    emitAdd(block, lhs, emitNeg(block, rhs))

  private def materializeAffineExpr(
      block: Block,
      expr: AffineExpr,
      dims: Map[String, Value[Attribute]],
      syms: Map[String, Value[Attribute]],
  ): Option[Value[Attribute]] =
    expr match
      case AffineConstantExpr(v) => Some(emitIndexConstant(block, v))
      case AffineDimExpr(name)   => dims.get(name)
      case AffineSymExpr(name)   => syms.get(name)
      case AffineBinaryOpExpr(AffineBinaryOp.Add, lhs, rhs) =>
        for
          l <- materializeAffineExpr(block, lhs, dims, syms)
          r <- materializeAffineExpr(block, rhs, dims, syms)
        yield emitAdd(block, l, r)
      case AffineBinaryOpExpr(AffineBinaryOp.Minus, lhs, rhs) =>
        for
          l <- materializeAffineExpr(block, lhs, dims, syms)
          r <- materializeAffineExpr(block, rhs, dims, syms)
        yield emitSub(block, l, r)
      case AffineBinaryOpExpr(AffineBinaryOp.Multiply, AffineConstantExpr(k), rhs) =>
        materializeAffineExpr(block, rhs, dims, syms).map(r =>
          emitMul(block, emitIndexConstant(block, k), r)
        )
      case AffineBinaryOpExpr(AffineBinaryOp.Multiply, lhs, AffineConstantExpr(k)) =>
        materializeAffineExpr(block, lhs, dims, syms).map(l =>
          emitMul(block, l, emitIndexConstant(block, k))
        )
      case AffineBinaryOpExpr(AffineBinaryOp.Multiply, _, _) => None
      case AffineBinaryOpExpr(AffineBinaryOp.CeilDiv | AffineBinaryOp.FloorDiv | AffineBinaryOp.Mod, _, _) =>
        None

  def materializeAffineMap(
      block: Block,
      operands: Seq[Value[Attribute]],
      map: AffineMapAttr,
  ): Option[Value[Attribute]] =
    if map.affineMap.affineExprs.size != 1 then None
    else
      val dimCount = map.affineMap.dimensions.size
      if operands.size != dimCount + map.affineMap.symbols.size then None
      else
        val dims = map.affineMap.dimensions.zip(operands.take(dimCount)).toMap
        val syms = map.affineMap.symbols.zip(operands.drop(dimCount)).toMap
        materializeAffineExpr(block, map.affineMap.affineExprs.head, dims, syms)

  def materializeAffineSet(
      block: Block,
      operands: Seq[Value[Attribute]],
      set: AffineSetAttr,
  ): Option[Value[Attribute]] =
    if set.affineSet.affineConstraints.size != 1 then None
    else
      val dimCount = set.affineSet.dimensions.size
      if operands.size != dimCount + set.affineSet.symbols.size then None
      else
        val dims = set.affineSet.dimensions.zip(operands.take(dimCount)).toMap
        val syms = set.affineSet.symbols.zip(operands.drop(dimCount)).toMap
        val constraint = set.affineSet.affineConstraints.head
        for
          lhs <- materializeAffineExpr(block, constraint.lhs, dims, syms)
          rhs <- materializeAffineExpr(block, constraint.rhs, dims, syms)
        yield
          val pred = constraint.kind match
            case AffineConstraintKind.LessEqual    => llvm.ICmpPredicate.sle
            case AffineConstraintKind.GreaterEqual => llvm.ICmpPredicate.sge
            case AffineConstraintKind.Equal        => llvm.ICmpPredicate.eq
          emitICmp(block, lhs, rhs, pred)

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

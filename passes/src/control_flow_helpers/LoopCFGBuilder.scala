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

def indexLikeAttr(v: BigInt, typ: IntegerType | IndexType): IntegerAttr =
  IntegerAttr(IntData(v), typ)

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
      case AffineDimExpr(name)   =>
        val idx = dims.indexOf(name)
        if idx < 0 || idx >= operands.size then None
        else Some(Right(operands(idx)))
      case _ => None

def explainUnsupporteDAffineExpr(expr: AffineExpr): Option[String] =
  expr match
    case _: AffineConstantExpr | _: AffineDimExpr | _: AffineSymExpr => None
    case AffineBinaryOpExpr(
          AffineBinaryOp.Add | AffineBinaryOp.Minus,
          lhs,
          rhs,
        ) =>
      explainUnsupporteDAffineExpr(lhs)
        .orElse(explainUnsupporteDAffineExpr(rhs))
    case AffineBinaryOpExpr(AffineBinaryOp.Multiply, lhs, rhs) =>
      (lhs, rhs) match
        case (_: AffineConstantExpr, _) => explainUnsupporteDAffineExpr(rhs)
        case (_, _: AffineConstantExpr) => explainUnsupporteDAffineExpr(lhs)
        case _                          =>
          Some(
            "multiplication is only supported when one operand is a constant"
          )
    case AffineBinaryOpExpr(
          AffineBinaryOp.CeilDiv | AffineBinaryOp.FloorDiv | AffineBinaryOp.Mod,
          _,
          _,
        ) =>
      Some(
        "ceildiv, floordiv, and mod affine expressions are not supported by refined CFG lowering"
      )

def explainUnsupporteDAffineMap(map: AffineMapAttr): Option[String] =
  if map.affineMap.affineExprs.size != 1 then
    Some(
      s"expected single-result affine map, got ${map.affineMap.affineExprs.size} results"
    )
  else explainUnsupporteDAffineExpr(map.affineMap.affineExprs.head)

final class LoopCFGBuilder(val blocks: mutable.ArrayBuffer[Block]):

  private def indexLikeType(v: Value[Attribute]): IntegerType | IndexType =
    v.typ match
      case t: IndexType   => t
      case t: IntegerType => t
      case _              => llvmIndexType

  private def preferredIndexType(
      values: Seq[Value[Attribute]]
  ): IntegerType | IndexType =
    values.collectFirst {
      case v if v.typ.isInstanceOf[IndexType] =>
        v.typ.asInstanceOf[IndexType]
    }.getOrElse(
      values.collectFirst {
        case v if v.typ.isInstanceOf[IntegerType] =>
          v.typ.asInstanceOf[IntegerType]
      }.getOrElse(llvmIndexType)
    )

  private def arithmeticResultType(
      lhs: Value[Attribute],
      rhs: Value[Attribute],
  ): IntegerType | IndexType =
    preferredIndexType(Seq(lhs, rhs))

  def appendBlock(block: Block): Unit =
    blocks += block

  def emitIndexConstant(block: Block, v: BigInt): Value[Attribute] =
    val c = llvm.Constant(llvmIndexAttr(v), Result(llvmIndexType))
    block.addOp(c)
    c.res

  def emitIndexConstant(
      block: Block,
      v: BigInt,
      typ: IntegerType | IndexType,
  ): Value[Attribute] =
    val c = llvm.Constant(indexLikeAttr(v, typ), Result(typ))
    block.addOp(c)
    c.res

  def emitIndexConstantLike(
      block: Block,
      v: BigInt,
      like: Value[Attribute],
  ): Value[Attribute] =
    emitIndexConstant(block, v, indexLikeType(like))

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
      Result(arithmeticResultType(lhs, rhs)),
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
      Result(arithmeticResultType(lhs, rhs)),
    )
    block.addOp(mul)
    mul.res

  def emitNeg(block: Block, value: Value[Attribute]): Value[Attribute] =
    val minusOne = emitIndexConstantLike(block, -1, value)
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
      preferredType: IntegerType | IndexType,
  ): Option[Value[Attribute]] =
    expr match
      case AffineConstantExpr(v) =>
        Some(emitIndexConstant(block, v, preferredType))
      case AffineDimExpr(name)                              => dims.get(name)
      case AffineSymExpr(name)                              => syms.get(name)
      case AffineBinaryOpExpr(AffineBinaryOp.Add, lhs, rhs) =>
        for
          l <- materializeAffineExpr(block, lhs, dims, syms, preferredType)
          r <- materializeAffineExpr(block, rhs, dims, syms, preferredType)
        yield emitAdd(block, l, r)
      case AffineBinaryOpExpr(AffineBinaryOp.Minus, lhs, rhs) =>
        for
          l <- materializeAffineExpr(block, lhs, dims, syms, preferredType)
          r <- materializeAffineExpr(block, rhs, dims, syms, preferredType)
        yield emitSub(block, l, r)
      case AffineBinaryOpExpr(
            AffineBinaryOp.Multiply,
            AffineConstantExpr(k),
            rhs,
          ) =>
        materializeAffineExpr(block, rhs, dims, syms, preferredType)
          .map(r => emitMul(block, emitIndexConstantLike(block, k, r), r))
      case AffineBinaryOpExpr(
            AffineBinaryOp.Multiply,
            lhs,
            AffineConstantExpr(k),
          ) =>
        materializeAffineExpr(block, lhs, dims, syms, preferredType)
          .map(l => emitMul(block, l, emitIndexConstantLike(block, k, l)))
      case AffineBinaryOpExpr(AffineBinaryOp.Multiply, _, _) => None
      case AffineBinaryOpExpr(
            AffineBinaryOp.CeilDiv | AffineBinaryOp.FloorDiv | AffineBinaryOp
              .Mod,
            _,
            _,
          ) =>
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
        materializeAffineExpr(
          block,
          map.affineMap.affineExprs.head,
          dims,
          syms,
          preferredIndexType(operands),
        )

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
        val preferredType = preferredIndexType(operands)
        for
          lhs <- materializeAffineExpr(
            block,
            constraint.lhs,
            dims,
            syms,
            preferredType,
          )
          rhs <- materializeAffineExpr(
            block,
            constraint.rhs,
            dims,
            syms,
            preferredType,
          )
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
    block
      .addOp(
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
    block
      .addOp(
        llvm.CondBr(
          asI1(cond),
          trueArgs.map(_.asInstanceOf[Operand[Attribute]]),
          falseArgs.map(_.asInstanceOf[Operand[Attribute]]),
          trueDest,
          falseDest,
        )
      )

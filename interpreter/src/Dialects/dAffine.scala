package scair.interpreter

import scair.dialects.affine.*
import scair.dialects.d_affine

private def asNatInt(x: Any, where: String): Int =
  x match
    case i: Int => i
    case _      => throw new Exception(s"$where expects Int nat values, got: $x")

private def evalAffineExpr(
    expr: AffineExpr,
    dims: Map[String, Int],
    syms: Map[String, Int],
): Int =
  expr match
    case AffineDimExpr(position) =>
      dims.getOrElse(
        position,
        throw new Exception(s"d_affine.apply missing dim operand for $position"),
      )
    case AffineSymExpr(position) =>
      syms.getOrElse(
        position,
        throw new Exception(s"d_affine.apply missing symbol operand for $position"),
      )
    case AffineConstantExpr(value) =>
      value.intValue
    case AffineBinaryOpExpr(op, lhs, rhs) =>
      val l = evalAffineExpr(lhs, dims, syms)
      val r = evalAffineExpr(rhs, dims, syms)
      op match
        case AffineBinaryOp.Add      => l + r
        case AffineBinaryOp.Minus    => l - r
        case AffineBinaryOp.Multiply => l * r
        case AffineBinaryOp.CeilDiv  => Math.floorDiv(l + r - 1, r)
        case AffineBinaryOp.FloorDiv => Math.floorDiv(l, r)
        case AffineBinaryOp.Mod      => Math.floorMod(l, r)

object run_d_affine_apply extends OpImpl[d_affine.Apply]:

  def compute(
      op: d_affine.Apply,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    val ints = args.map(asNatInt(_, "d_affine.apply operand"))
    val dimCount = op.map.affineMap.dimensions.size
    val symCount = op.map.affineMap.symbols.size
    if ints.size != dimCount + symCount then
      throw new Exception(
        s"d_affine.apply expected ${dimCount + symCount} operands, got ${ints.size}"
      )
    val dims = op.map.affineMap.dimensions.zip(ints.take(dimCount)).toMap
    val syms = op.map.affineMap.symbols.zip(ints.drop(dimCount)).toMap
    val exprs = op.map.affineMap.affineExprs
    if exprs.size != 1 then
      throw new Exception("d_affine.apply currently supports single-result affine maps only")
    Some(evalAffineExpr(exprs.head, dims, syms))

object run_d_affine_for extends OpImpl[d_affine.For]:

  def compute(
      op: d_affine.For,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(lbAny, ubAny) =>
        val lb = asNatInt(lbAny, "d_affine.for lb")
        val ub = asNatInt(ubAny, "d_affine.for ub")
        val step = op.step.value.value.toInt
        val bodyBlock = op.body.blocks.head
        val iv = bodyBlock.arguments.head
        var i = lb
        while i < ub do
          ctx.scopedDict.update(iv, i)
          bodyBlock.operations.foreach(interpreter.interpret_op(_, ctx))
          i += step
        None
      case _ =>
        throw new Exception("d_affine.for expects (lb, ub)")

object run_d_affine_yield extends OpImpl[d_affine.Yield]:

  def compute(
      op: d_affine.Yield,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    None

object run_d_affine_min extends OpImpl[d_affine.Min]:

  def compute(
      op: d_affine.Min,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(a, b) =>
        Some(math.min(asNatInt(a, "d_affine.min lhs"), asNatInt(b, "d_affine.min rhs")))
      case _ =>
        throw new Exception("d_affine.min expects exactly two nat operands")

val InterpreterdAffineDialect: InterpreterDialect =
  Seq(
    run_d_affine_apply,
    run_d_affine_for,
    run_d_affine_yield,
    run_d_affine_min,
  )

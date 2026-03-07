package scair.interpreter

import scair.dialects.d_affine

private def asNatInt(x: Any, where: String): Int =
  x match
    case i: Int => i
    case _      => throw new Exception(s"$where expects Int nat values, got: $x")

object run_d_affine_for extends OpImpl[d_affine.For]:

  def compute(
      op: d_affine.For,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(lbAny, ubAny, stepAny) =>
        val lb = asNatInt(lbAny, "d_affine.for lb")
        val ub = asNatInt(ubAny, "d_affine.for ub")
        val step = asNatInt(stepAny, "d_affine.for step")
        val bodyBlock = op.body.blocks.head
        val iv = bodyBlock.arguments.head
        var i = lb
        while i < ub do
          ctx.scopedDict.update(iv, i)
          bodyBlock.operations.foreach(interpreter.interpret_op(_, ctx))
          i += step
        None
      case _ =>
        throw new Exception("d_affine.for expects (lb, ub, step)")

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
    run_d_affine_for,
    run_d_affine_yield,
    run_d_affine_min,
  )

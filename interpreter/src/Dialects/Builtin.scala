package scair.interpreter

import scair.dialects.builtin.UnrealizedConversionCastOp

object run_unrealized_conversion_cast
    extends OpImpl[UnrealizedConversionCastOp]:

  def compute(
      op: UnrealizedConversionCastOp,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    op.outputs.size match
      case 0 => None
      case 1 =>
        if args.size == 1 then Some(args.head)
        else Some(args)
      case n =>
        Some(args.take(n))

val InterpreterBuiltinDialect: InterpreterDialect =
  Seq(run_unrealized_conversion_cast)

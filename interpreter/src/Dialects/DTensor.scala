package scair.interpreter

import scair.dialects.{d_tensor as DTensor}

object run_nat_const extends OpImpl[DTensor.NatConst]:

  def compute(
      op: DTensor.NatConst,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    Some(op.value.value.toInt)

object run_shape_to_index extends OpImpl[DTensor.ShapeToIndex]:

  def compute(
      op: DTensor.ShapeToIndex,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(v: Int) => Some(v)
      case _           => throw new Exception("d_tensor.shape.to_index expects a nat Int operand")

val InterpreterDTensorDialect: InterpreterDialect =
  Seq(
    run_nat_const,
    run_shape_to_index,
  )

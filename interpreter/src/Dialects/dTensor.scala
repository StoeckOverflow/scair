package scair.interpreter

import scair.dialects.dTensor

object run_nat_const extends OpImpl[dTensor.NatConst]:

  def compute(
      op: dTensor.NatConst,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    Some(op.value.value.toInt)

object run_shape_to_index extends OpImpl[dTensor.ShapeToIndex]:

  def compute(
      op: dTensor.ShapeToIndex,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(v: Int) => Some(v)
      case _           => throw new Exception("dtensor.shape.to_index expects a nat Int operand")

val InterpreterdTensorDialect: InterpreterDialect =
  Seq(
    run_nat_const,
    run_shape_to_index,
  )

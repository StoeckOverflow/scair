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

object run_nat_param extends OpImpl[dTensor.NatParam]:

  def compute(
      op: dTensor.NatParam,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    Some(0)

object run_nat_add extends OpImpl[dTensor.NatAdd]:

  def compute(
      op: dTensor.NatAdd,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(lhs: Int, rhs: Int) => Some(lhs + rhs)
      case _                       => throw new Exception("dtensor.nat.add expects Int operands")

object run_nat_mul extends OpImpl[dTensor.NatMul]:

  def compute(
      op: dTensor.NatMul,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(lhs: Int, rhs: Int) => Some(lhs * rhs)
      case _                       => throw new Exception("dtensor.nat.mul expects Int operands")

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
    run_nat_param,
    run_nat_add,
    run_nat_mul,
    run_shape_to_index,
  )

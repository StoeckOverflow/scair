package scair.interpreter

import scair.dialects.{d_tensor as DTensor}

object run_size_constant extends OpImpl[DTensor.SizeConstant]:

  def compute(
      op: DTensor.SizeConstant,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    Some(op.value.value.toInt)

object run_size_import extends OpImpl[DTensor.SizeImport]:

  def compute(
      op: DTensor.SizeImport,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(v: Int) => Some(v)
      case _           => throw new Exception("d_tensor.size.import expects an index Int operand")

object run_size_add extends OpImpl[DTensor.SizeAdd]:

  def compute(
      op: DTensor.SizeAdd,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(lhs: Int, rhs: Int) => Some(lhs + rhs)
      case _                       => throw new Exception("d_tensor.size.add expects two Int operands")

object run_size_mul extends OpImpl[DTensor.SizeMul]:

  def compute(
      op: DTensor.SizeMul,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(lhs: Int, rhs: Int) => Some(lhs * rhs)
      case _                       => throw new Exception("d_tensor.size.mul expects two Int operands")

object run_size_positive_proof extends OpImpl[DTensor.SizePositiveProof]:

  def compute(
      op: DTensor.SizePositiveProof,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(_: Int, proof: Boolean) => Some(proof)
      case _                           => throw new Exception("d_tensor.size.positive_proof expects a size and i1 proof")

object run_size_refine_positive extends OpImpl[DTensor.SizeRefinePositive]:

  def compute(
      op: DTensor.SizeRefinePositive,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(size: Int, true) => Some(size)
      case Seq(_: Int, false)  => throw new Exception("d_tensor.size.refine_positive received a false proof")
      case _                   => throw new Exception("d_tensor.size.refine_positive expects a size and proof")

val InterpreterDTensorDialect: InterpreterDialect =
  Seq(
    run_size_constant,
    run_size_import,
    run_size_add,
    run_size_mul,
    run_size_positive_proof,
    run_size_refine_positive,
  )

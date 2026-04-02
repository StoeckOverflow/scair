package scair.interpreter

import scair.dialects.affine.*
import scair.dialects.builtin.*
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

private def evalSingleResultMap(
    map: AffineMapAttr,
    operands: Seq[Int],
    where: String,
): Int =
  val dimCount = map.affineMap.dimensions.size
  val symCount = map.affineMap.symbols.size
  if operands.size != dimCount + symCount then
    throw new Exception(
      s"$where expected ${dimCount + symCount} operands, got ${operands.size}"
    )
  val exprs = map.affineMap.affineExprs
  if exprs.size != 1 then
    throw new Exception(s"$where currently supports single-result affine maps only")
  val dims = map.affineMap.dimensions.zip(operands.take(dimCount)).toMap
  val syms = map.affineMap.symbols.zip(operands.drop(dimCount)).toMap
  evalAffineExpr(exprs.head, dims, syms)

object run_d_affine_apply extends OpImpl[d_affine.Apply]:

  def compute(
      op: d_affine.Apply,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    val dimCount = op.dimOperands.size
    val symCount = op.symbolOperands.size
    if args.size != dimCount + symCount then
      throw new Exception(
        s"d_affine.apply expected ${dimCount + symCount} operands, got ${args.size}"
      )
    val dimInts = args.take(dimCount).map(asNatInt(_, "d_affine.apply dim operand"))
    val symInts = args.drop(dimCount).map(asNatInt(_, "d_affine.apply symbol operand"))
    Some(evalSingleResultMap(op.map, dimInts ++ symInts, "d_affine.apply"))

object run_d_affine_for extends OpImpl[d_affine.For]:

  def compute(
      op: d_affine.For,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    val lbCount = op.lowerBoundOperands.size
    val ubCount = op.upperBoundOperands.size
    val minExpected = lbCount + ubCount
    if args.size < minExpected then
      throw new Exception(
        s"d_affine.for expects at least $minExpected bound operands"
      )
    else
      val lbInts = args.take(lbCount).map(asNatInt(_, "d_affine.for lower bound operand"))
      val ubInts = args.slice(lbCount, lbCount + ubCount).map(
        asNatInt(_, "d_affine.for upper bound operand")
      )
      val lb = evalSingleResultMap(op.lowerBoundMap, lbInts, "d_affine.for lower bound")
      val ub = evalSingleResultMap(op.upperBoundMap, ubInts, "d_affine.for upper bound")
      val step = op.step.value.value.toInt
      val bodyBlock = op.body.blocks.head
      val iv = bodyBlock.arguments.head
      val iterArgs = bodyBlock.arguments.tail
      val term = bodyBlock.operations.lastOption match
        case Some(y: d_affine.Yield) => y
        case Some(other) =>
          throw new Exception(s"d_affine.for expects terminator d_affine.yield, got `${other.name}`")
        case None =>
          throw new Exception("d_affine.for expects non-empty loop body")

      var carried = args.drop(minExpected)
      var i = lb
      while i < ub do
        ctx.scopedDict.update(iv, i)
        for (iterArg, value) <- iterArgs.zip(carried) do
          ctx.scopedDict.update(iterArg, value)

        bodyBlock.operations.dropRight(1).foreach(interpreter.interpret_op(_, ctx))
        carried = term.args.map(arg => interpreter.lookup_op(arg, ctx))
        i += step

      op.res.size match
        case 0 => None
        case 1 => Some(carried.head)
        case _ => Some(carried)

object run_d_affine_yield extends OpImpl[d_affine.Yield]:

  def compute(
      op: d_affine.Yield,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    None

val InterpreterdAffineDialect: InterpreterDialect =
  Seq(
    run_d_affine_apply,
    run_d_affine_for,
    run_d_affine_yield,
  )

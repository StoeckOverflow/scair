package scair.interpreter

import scair.dialects.d_memref

private def asInt(x: Any, name: String): Int =
  x match
    case i: Int => i
    case _      => throw new Exception(s"$name must be an Int, got: $x")

object run_d_alloc extends OpImpl[d_memref.Alloc]:

  def compute(
      op: d_memref.Alloc,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    val shape = op.res.typ.params.map(d =>
      asInt(interpreter.lookup_op(d.getVal(), ctx), "d_memref.alloc dim")
    )
    Some(ShapedArray(shape))

object run_d_dealloc extends OpImpl[d_memref.Dealloc]:

  def compute(
      op: d_memref.Dealloc,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    None

object run_d_store extends OpImpl[d_memref.Store]:

  def compute(
      op: d_memref.Store,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(value, memref: ShapedArray, indices*) =>
        memref(indices.map(asInt(_, "d_memref.store index"))) = value
        None
      case _ =>
        throw new Exception("d_memref.store expects (value, ShapedArray, indices...)")

object run_d_load extends OpImpl[d_memref.Load]:

  def compute(
      op: d_memref.Load,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(memref: ShapedArray, indices*) =>
        Some(memref(indices.map(asInt(_, "d_memref.load index"))))
      case _ =>
        throw new Exception("d_memref.load expects (ShapedArray, indices...)")

object run_d_dim extends OpImpl[d_memref.Dim]:

  def compute(
      op: d_memref.Dim,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    val axis = op.axis.value.toInt
    val dimVal = op.memref.typ.params(axis).getVal()
    Some(asInt(interpreter.lookup_op(dimVal, ctx), "d_memref.dim result"))

object run_d_cast extends OpImpl[d_memref.Cast]:

  def compute(
      op: d_memref.Cast,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(memref: ShapedArray) => Some(memref)
      case _                        => throw new Exception("d_memref.cast expects a ShapedArray")

object run_d_subview extends OpImpl[d_memref.Subview]:

  def compute(
      op: d_memref.Subview,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(src: ShapedArray, rest*) =>
        val rank = op.res.typ.params.size
        val all = rest.map(asInt(_, "d_memref.subview operand"))
        val offsets = all.take(rank)
        val sizes = all.drop(rank).take(rank)
        Some(src.subview(offsets, sizes))
      case _ =>
        throw new Exception("d_memref.subview expects (ShapedArray, offsets..., sizes...)")

val InterpreterdMemrefDialect: InterpreterDialect =
  Seq(
    run_d_alloc,
    run_d_dealloc,
    run_d_store,
    run_d_load,
    run_d_dim,
    run_d_cast,
    run_d_subview,
  )

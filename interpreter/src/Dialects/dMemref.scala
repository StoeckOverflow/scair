package scair.interpreter

import scair.dialects.builtin.*
import scair.dialects.d_memref
import scair.ir.ValueAttribute

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
    args match
      case Seq(memref: ShapedArray, axisAny) =>
        val axis = asInt(axisAny, "d_memref.dim axis")
        if axis < 0 || axis >= memref.shape.length then
          throw new Exception(
            s"d_memref.dim axis out of bounds: axis=$axis rank=${memref.shape.length}"
          )
        Some(memref.shape(axis))
      case _ =>
        throw new Exception("d_memref.dim expects (ShapedArray, axis:index)")

object run_d_dim_exact extends OpImpl[d_memref.DimExact]:

  def compute(
      op: d_memref.DimExact,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    val axis = op.axis.value.toInt
    val dimVal = op.memref.typ.params(axis).getVal()
    Some(asInt(interpreter.lookup_op(dimVal, ctx), "d_memref.dim_exact result"))

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
        val strides = all.drop(2 * rank).take(rank)
        Some(src.subview(offsets, sizes, strides))
      case _ =>
        throw new Exception(
          "d_memref.subview expects (ShapedArray, offsets..., sizes..., strides...)"
        )

object run_d_reinterpret_cast extends OpImpl[d_memref.ReinterpretCast]:

  private def asLayoutInt(
      param: d_memref.LayoutParam,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      name: String,
  ): Int =
    param match
      case i: IntegerAttr    => i.value.value.toInt
      case v: ValueAttribute => asInt(interpreter.lookup_op(v.getVal(), ctx), name)

  def compute(
      op: d_memref.ReinterpretCast,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(src: ShapedArray) =>
        val rank = op.res.typ.params.size
        val offset = asLayoutInt(
          op.res.typ.offset.get,
          interpreter,
          ctx,
          "d_memref.reinterpret_cast offset",
        )
        val sizes = op.res.typ.params.map(d =>
          asInt(interpreter.lookup_op(d.getVal(), ctx), "d_memref.reinterpret_cast size")
        )
        val strides = op.res.typ.strides.get.map(s =>
          asLayoutInt(s, interpreter, ctx, "d_memref.reinterpret_cast stride")
        )
        Some(src.reinterpret(offset, sizes, strides))
      case _ =>
        throw new Exception(
          "d_memref.reinterpret_cast expects (ShapedArray)"
        )

val InterpreterdMemrefDialect: InterpreterDialect =
  Seq(
    run_d_alloc,
    run_d_dealloc,
    run_d_store,
    run_d_load,
    run_d_dim,
    run_d_dim_exact,
    run_d_cast,
    run_d_subview,
    run_d_reinterpret_cast,
  )

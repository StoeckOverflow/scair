package scair.dialects.tensor

import scair.Printer
import scair.clair.macros.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.utils.*

private object TensorOpVerify:
  private def renderAttr(a: Attribute): String =
    val out = java.io.StringWriter()
    val printer = Printer(p = java.io.PrintWriter(out))
    printer.print(a)
    printer.flush()
    out.toString

  private def sameDims(lhs: Seq[DimParam], rhs: Seq[DimParam]): Boolean =
    lhs.size == rhs.size &&
    lhs.zip(rhs).forall((l, r) => l.getVal() == r.getVal())

  private def checkSameTensorShapeAndElem(
      lhs: TensorTensorType,
      rhs: TensorTensorType,
      opName: String,
      lhsName: String,
      rhsName: String,
  ): OK[Unit] =
    if lhs.elem != rhs.elem then
      Err(
        s"$opName: expected equal element types for $lhsName/$rhsName, got ${renderAttr(lhs.elem)} and ${renderAttr(rhs.elem)}"
      )
    else if lhs.params.size != rhs.params.size then
      Err(
        s"$opName: expected equal ranks for $lhsName/$rhsName, got ${lhs.params.size} and ${rhs.params.size}"
      )
    else if !sameDims(lhs.params, rhs.params) then
      Err(
        s"$opName: expected pairwise SSA-identical dims for $lhsName/$rhsName, got ${renderAttr(lhs)} and ${renderAttr(rhs)}"
      )
    else OK(())

  def checkTensorElementwise(
      lhs: TensorTensorType,
      rhs: TensorTensorType,
      res: TensorTensorType,
      opName: String,
  ): OK[Unit] =
    checkSameTensorShapeAndElem(lhs, rhs, opName, "lhs", "rhs").flatMap(_ =>
      checkSameTensorShapeAndElem(lhs, res, opName, "lhs", "result")
    )

  def checkMatmul(
      lhs: TensorTensorType,
      rhs: TensorTensorType,
      res: TensorTensorType,
  ): OK[Unit] =
    if lhs.params.size != 2 || rhs.params.size != 2 then
      Err(
        s"tensor.matmul: expected rank-2 operands, got rank ${lhs.params.size} and ${rhs.params.size}"
      )
    else if res.params.size != 2 then
      Err(
        s"tensor.matmul: expected rank-2 result, got rank ${res.params.size}"
      )
    else if lhs.elem != rhs.elem || lhs.elem != res.elem then
      Err(
        s"tensor.matmul: expected equal element types for lhs/rhs/result, got ${renderAttr(lhs.elem)}, ${renderAttr(rhs.elem)}, ${renderAttr(res.elem)}"
      )
    else if lhs.params(1).getVal() != rhs.params(0).getVal() then
      Err("tensor.matmul: expected SSA-identical inner dims (lhs.k === rhs.k)")
    else if lhs.params(0).getVal() != res.params(0).getVal() ||
      rhs.params(1).getVal() != res.params(1).getVal()
    then
      Err("tensor.matmul: expected result dims to be outer dims (lhs.m, rhs.n)")
    else OK(())

final case class NatConst(
    value: IntegerAttr,
    res: Result[TensorNatType],
) extends DerivedOperation["tensor.nat.const", NatConst]
    with NoMemoryEffect
    derives DerivedOperationCompanion:
  override def customVerify(): OK[Operation] =
    if value.value.value >= 0 then OK(this)
    else Err(s"tensor.nat.const: expected non-negative literal, got $value")

final case class NatAdd(
    lhs: Operand[TensorNatType],
    rhs: Operand[TensorNatType],
    res: Result[TensorNatType],
) extends DerivedOperation["tensor.nat.add", NatAdd]
    with NoMemoryEffect
    derives DerivedOperationCompanion

final case class NatMul(
    lhs: Operand[TensorNatType],
    rhs: Operand[TensorNatType],
    res: Result[TensorNatType],
) extends DerivedOperation["tensor.nat.mul", NatMul]
    with NoMemoryEffect
    derives DerivedOperationCompanion

final case class Empty(
    res: Result[TensorTensorType],
) extends DerivedOperation["tensor.empty", Empty]
    with NoMemoryEffect
    derives DerivedOperationCompanion

final case class Fill(
    v: Operand[TypeAttribute],
    res: Result[TensorTensorType],
) extends DerivedOperation["tensor.fill", Fill]
    with NoMemoryEffect
    derives DerivedOperationCompanion:
  override def customVerify(): OK[Operation] =
    if v.typ == res.typ.elem then OK(this)
    else
      Err(
        s"tensor.fill: expected fill value type ${res.typ.elem}, got ${v.typ}"
      )

final case class Dim(
    t: Operand[TensorTensorType],
    axis: IntegerAttr,
    res: Result[TensorNatType],
) extends DerivedOperation["tensor.dim", Dim]
    with NoMemoryEffect
    derives DerivedOperationCompanion:
  def selectedDimValue: OK[Value[Attribute]] =
    val idx = axis.value.value
    val rank = BigInt(t.typ.params.size)
    if idx < 0 || idx >= rank then
      Err(s"tensor.dim: axis $idx out of bounds for rank ${t.typ.params.size}")
    else OK(t.typ.params(idx.toInt).getVal())

  override def customVerify(): OK[Operation] =
    val axisTyOk = axis.typ == I32
    if !axisTyOk then
      Err(s"tensor.dim: expected i32 axis attribute, got ${axis.typ}")
    else
      selectedDimValue.map(_ => this)

final case class Add(
    lhs: Operand[TensorTensorType],
    rhs: Operand[TensorTensorType],
    res: Result[TensorTensorType],
) extends DerivedOperation["tensor.add", Add]
    with NoMemoryEffect
    derives DerivedOperationCompanion:
  override def customVerify(): OK[Operation] =
    TensorOpVerify
      .checkTensorElementwise(lhs.typ, rhs.typ, res.typ, "tensor.add")
      .map(_ => this)

final case class Mul(
    lhs: Operand[TensorTensorType],
    rhs: Operand[TensorTensorType],
    res: Result[TensorTensorType],
) extends DerivedOperation["tensor.mul", Mul]
    with NoMemoryEffect
    derives DerivedOperationCompanion:
  override def customVerify(): OK[Operation] =
    TensorOpVerify
      .checkTensorElementwise(lhs.typ, rhs.typ, res.typ, "tensor.mul")
      .map(_ => this)

final case class Matmul(
    lhs: Operand[TensorTensorType],
    rhs: Operand[TensorTensorType],
    res: Result[TensorTensorType],
) extends DerivedOperation["tensor.matmul", Matmul]
    with NoMemoryEffect
    derives DerivedOperationCompanion:
  override def customVerify(): OK[Operation] =
    TensorOpVerify.checkMatmul(lhs.typ, rhs.typ, res.typ).map(_ => this)

final case class Cast(
    src: Operand[TensorTensorType],
    res: Result[TensorTensorType],
) extends DerivedOperation["tensor.cast", Cast]
    with NoMemoryEffect
    derives DerivedOperationCompanion:
  override def customVerify(): OK[Operation] =
    if src.typ.elem != res.typ.elem then
      Err(
        s"tensor.cast: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if src.typ.params.size != res.typ.params.size then
      Err(
        s"tensor.cast: expected equal ranks, got ${src.typ.params.size} and ${res.typ.params.size}"
      )
    else if src.typ.params
        .zip(res.typ.params)
        .exists((s, r) => s.getVal() != r.getVal())
    then Err("tensor.cast: expected pairwise SSA-identical dims")
    else OK(this)

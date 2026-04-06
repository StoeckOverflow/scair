package scair.dialects.dTensor

import scair.Printer
import scair.clair.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.utils.*

final case class NatParam(
    res: Result[dTensorNatType]
) extends DerivedOperation["dtensor.nat.param"] derives OpDefs

final case class NatConst(
    value: IntegerAttr,
    res: Result[dTensorNatType],
) extends DerivedOperation["dtensor.nat.const"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if value.value.value >= 0 then OK(this)
    else Err(s"dtensor.nat.const: expected non-negative literal, got $value")

final case class NatAdd(
    lhs: Operand[dTensorNatType],
    rhs: Operand[dTensorNatType],
    res: Result[dTensorNatType],
) extends DerivedOperation["dtensor.nat.add"]
    with NoMemoryEffect derives OpDefs

final case class NatMul(
    lhs: Operand[dTensorNatType],
    rhs: Operand[dTensorNatType],
    res: Result[dTensorNatType],
) extends DerivedOperation["dtensor.nat.mul"]
    with NoMemoryEffect derives OpDefs

final case class ShapeToIndex(
    nat: Operand[dTensorNatType],
    res: Result[IndexType],
) extends DerivedOperation["dtensor.shape.to_index"]
    with NoMemoryEffect derives OpDefs

final case class IndexToNat(
    index: Operand[IndexType],
    res: Result[dTensorNatType],
) extends DerivedOperation["dtensor.index_to_nat"]
    with NoMemoryEffect derives OpDefs

final case class Empty(
    res: Result[dTensorTensorType]
) extends DerivedOperation["dtensor.empty"]
    with NoMemoryEffect derives OpDefs

final case class Fill(
    v: Operand[TypeAttribute],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.fill"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if v.typ == res.typ.elem then OK(this)
    else
      Err(
        s"dtensor.fill: expected fill value type ${res.typ.elem}, got ${v.typ}"
      )

final case class Dim(
    t: Operand[dTensorTensorType],
    axis: IntegerAttr,
    res: Result[ValueRefType],
) extends DerivedOperation["dtensor.dim"]
    with NoMemoryEffect derives OpDefs:

  def selectedDimValue: OK[Value[Attribute]] =
    val idx = axis.value.value
    val rank = BigInt(t.typ.params.size)
    if idx < 0 || idx >= rank then
      Err(s"dtensor.dim: axis $idx out of bounds for rank ${t.typ.params.size}")
    else OK(t.typ.params(idx.toInt).getVal())

  override def customVerify(): OK[Operation] =
    val axisTyOk = axis.typ == I32
    if !axisTyOk then
      Err(s"dtensor.dim: expected i32 axis attribute, got ${axis.typ}")
    else
      selectedDimValue.flatMap(sel =>
        if res.typ.ref.getVal() eq sel then
          dTensorTypeUtil.resolveNatValue(res.typ.ref.getVal()).map(_ => this)
        else
          Err(
            "dtensor.dim: expected result !value<...> to reference the selected embedded dim"
          )
      )

final case class Add(
    lhs: Operand[dTensorTensorType],
    rhs: Operand[dTensorTensorType],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.add"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    dTensorTypeUtil
      .checkTensorElementwise(lhs.typ, rhs.typ, res.typ, "dtensor.add")
      .map(_ => this)

final case class Mul(
    lhs: Operand[dTensorTensorType],
    rhs: Operand[dTensorTensorType],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.mul"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    dTensorTypeUtil
      .checkTensorElementwise(lhs.typ, rhs.typ, res.typ, "dtensor.mul")
      .map(_ => this)

final case class Matmul(
    lhs: Operand[dTensorTensorType],
    rhs: Operand[dTensorTensorType],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.matmul"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    dTensorTypeUtil.checkMatmul(lhs.typ, rhs.typ, res.typ).map(_ => this)

final case class Cast(
    src: Operand[dTensorTensorType],
    res: Result[dTensorTensorType],
) extends DerivedOperation["dtensor.cast"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if src.typ.elem != res.typ.elem then
      Err(
        s"dtensor.cast: expected equal element types, got ${src.typ
            .elem} and ${res.typ.elem}"
      )
    else if src.typ.params.size != res.typ.params.size then
      Err(
        s"dtensor.cast: expected equal ranks, got ${src.typ.params
            .size} and ${res.typ.params.size}"
      )
    else if !dTensorTypeUtil.sameDims(src.typ.params, res.typ.params)
    then Err("dtensor.cast: expected pairwise SSA-identical dims")
    else OK(this)

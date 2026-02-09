package scair.dialects.tensor

import scair.clair.macros.*
import scair.ir.*
import scair.utils.*

final case class VAdd(
    lhs: Value[TensorVectorType],
    rhs: Value[TensorVectorType],
    res: Result[TensorVectorType],
) extends DerivedOperation["tensor.vadd", VAdd]
    derives DerivedOperationCompanion:
  override def verify(): OK[Operation] =
    if lhs.typ == rhs.typ && lhs.typ == res.typ then OK(this)
    else
      Err(
        s"vadd: expected lhs/rhs/res to have the same vector type, got ${lhs.typ}, ${rhs.typ}, ${res.typ}"
      )

final case class MAdd(
    lhs: Value[TensorMatrixType],
    rhs: Value[TensorMatrixType],
    res: Result[TensorMatrixType],
) extends DerivedOperation["tensor.madd", MAdd]
    derives DerivedOperationCompanion:
  override def verify(): OK[Operation] =
    if lhs.typ == rhs.typ && lhs.typ == res.typ then OK(this)
    else
      Err(
        s"madd: expected lhs/rhs/res to have the same matrix type, got ${lhs.typ}, ${rhs.typ}, ${res.typ}"
      )

final case class TAdd(
    lhs: Value[TensorTensorType],
    rhs: Value[TensorTensorType],
    res: Result[TensorTensorType],
) extends DerivedOperation["tensor.tadd", TAdd]
    derives DerivedOperationCompanion:
  override def verify(): OK[Operation] =
    if lhs.typ == rhs.typ && lhs.typ == res.typ then OK(this)
    else
      Err(
        s"tadd: expected lhs/rhs/res to have the same tensor type, got ${lhs.typ}, ${rhs.typ}, ${res.typ}"
      )

final case class VMul(
    lhs: Value[TensorVectorType],
    rhs: Value[TensorVectorType],
    res: Result[TensorVectorType],
) extends DerivedOperation["tensor.vmul", VMul]
    derives DerivedOperationCompanion:
  override def verify(): OK[Operation] =
    if lhs.typ == rhs.typ && lhs.typ == res.typ then OK(this)
    else
      Err(
        s"vmul: expected lhs/rhs/res to have the same vector type, got ${lhs.typ}, ${rhs.typ}, ${res.typ}"
      )

final case class MMul(
    lhs: Value[TensorMatrixType],
    rhs: Value[TensorMatrixType],
    res: Result[TensorMatrixType],
) extends DerivedOperation["tensor.mmul", MMul]
    derives DerivedOperationCompanion:
  override def verify(): OK[Operation] =
    val TensorMatrixType(lRows, lCols, lElem) = lhs.typ
    val TensorMatrixType(rRows, rCols, rElem) = rhs.typ
    val TensorMatrixType(oRows, oCols, oElem) = res.typ
    if lElem == rElem && lElem == oElem &&
      lCols == rRows &&
      lRows == oRows &&
      rCols == oCols
    then OK(this)
    else
      Err(
        s"mmul: expected (r x k, k x c) -> (r x c) with same element type, got ${lhs.typ}, ${rhs.typ}, ${res.typ}"
      )

final case class TMul(
    lhs: Value[TensorTensorType],
    rhs: Value[TensorTensorType],
    res: Result[TensorTensorType],
) extends DerivedOperation["tensor.tmul", TMul]
    derives DerivedOperationCompanion:
  override def verify(): OK[Operation] =
    if lhs.typ == rhs.typ && lhs.typ == res.typ then OK(this)
    else
      Err(
        s"tmul: expected lhs/rhs/res to have the same tensor type, got ${lhs.typ}, ${rhs.typ}, ${res.typ}"
      )

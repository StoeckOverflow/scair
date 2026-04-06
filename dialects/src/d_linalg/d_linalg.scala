package scair.dialects.d_linalg

import scair.clair.*
import scair.dialects.dTensor.*
import scair.dialects.d_memref
import scair.dialects.d_memref.dMemrefTypeUtil
import scair.ir.*
import scair.utils.*

private def isSupportedContainerType(attr: Attribute): Boolean =
  attr match
    case _: dTensorTensorType          => true
    case _: d_memref.dMemrefMemrefType => true
    case _                             => false

private def sameContainerKind(lhs: Attribute, rhs: Attribute): Boolean =
  (lhs, rhs) match
    case (_: dTensorTensorType, _: dTensorTensorType)                   => true
    case (_: d_memref.dMemrefMemrefType, _: d_memref.dMemrefMemrefType) => true
    case _                                                              => false

private def containerElem(attr: Attribute): Option[TypeAttribute] =
  attr match
    case t: dTensorTensorType          => Some(t.elem)
    case m: d_memref.dMemrefMemrefType => Some(m.elem)
    case _                             => None

private def containerRank(attr: Attribute): Option[Int] =
  attr match
    case t: dTensorTensorType          => Some(t.params.size)
    case m: d_memref.dMemrefMemrefType => Some(m.params.size)
    case _                             => None

private def sameShape(lhs: Attribute, rhs: Attribute): Boolean =
  (lhs, rhs) match
    case (l: dTensorTensorType, r: dTensorTensorType) =>
      dTensorTypeUtil.sameDims(l.params, r.params)
    case (l: d_memref.dMemrefMemrefType, r: d_memref.dMemrefMemrefType) =>
      dMemrefTypeUtil.sameDims(l.params, r.params)
    case _ => false

private def matmulShapeOk(
    lhs: Attribute,
    rhs: Attribute,
    out: Attribute,
): Boolean =
  (lhs, rhs, out) match
    case (l: dTensorTensorType, r: dTensorTensorType, o: dTensorTensorType) =>
      dTensorTypeUtil.checkMatmul(l, r, o) match
        case OK(_) => true
        case _     => false
    case (
          l: d_memref.dMemrefMemrefType,
          r: d_memref.dMemrefMemrefType,
          o: d_memref.dMemrefMemrefType,
        ) =>
      l.params.size == 2 &&
      r.params.size == 2 &&
      o.params.size == 2 &&
      l.elem == r.elem &&
      l.elem == o.elem &&
      dMemrefTypeUtil.sameDims(Seq(l.params(1)), Seq(r.params(0))) &&
      dMemrefTypeUtil.sameDims(Seq(l.params(0)), Seq(o.params(0))) &&
      dMemrefTypeUtil.sameDims(Seq(r.params(1)), Seq(o.params(1)))
    case _ => false

final case class Fill(
    value: Operand[TypeAttribute],
    out: Operand[Attribute],
    res: Seq[Result[Attribute]],
) extends DerivedOperation["d_linalg.fill"]
    derives OpDefs:

  override def customVerify(): OK[Operation] =
    if !isSupportedContainerType(out.typ) then
      Err(s"d_linalg.fill: expected dtensor or d_memref output, got ${out.typ}")
    else if containerElem(out.typ) != Some(value.typ) then
      Err(
        s"d_linalg.fill: expected fill value type ${containerElem(out.typ).get}, got ${value.typ}"
      )
    else
      out.typ match
        case t: dTensorTensorType =>
          if res.size != 1 then
            Err(s"d_linalg.fill: tensor form expects 1 result, got ${res.size}")
          else if res.head.typ != t then
            Err(s"d_linalg.fill: tensor form result type must be ${t}, got ${res.head.typ}")
          else OK(this)
        case _: d_memref.dMemrefMemrefType =>
          if res.nonEmpty then
            Err(s"d_linalg.fill: memref form expects 0 results, got ${res.size}")
          else OK(this)
        case _ => Err(s"d_linalg.fill: unsupported output type ${out.typ}")

final case class Matmul(
    lhs: Operand[Attribute],
    rhs: Operand[Attribute],
    out: Operand[Attribute],
    res: Seq[Result[Attribute]],
) extends DerivedOperation["d_linalg.matmul"]
    derives OpDefs:

  override def customVerify(): OK[Operation] =
    if !isSupportedContainerType(lhs.typ) || !isSupportedContainerType(rhs.typ) || !isSupportedContainerType(
        out.typ
      )
    then Err(s"d_linalg.matmul: expected dtensor or d_memref operands, got ${lhs.typ}, ${rhs.typ}, ${out.typ}")
    else if !sameContainerKind(lhs.typ, rhs.typ) || !sameContainerKind(lhs.typ, out.typ) then
      Err("d_linalg.matmul: lhs/rhs/out must all use the same container kind")
    else if containerRank(lhs.typ) != Some(2) || containerRank(rhs.typ) != Some(
        2
      ) || containerRank(out.typ) != Some(2)
    then Err("d_linalg.matmul: expected rank-2 lhs/rhs/out")
    else if !matmulShapeOk(lhs.typ, rhs.typ, out.typ) then
      Err("d_linalg.matmul: expected matmul-compatible shapes")
    else
      out.typ match
        case t: dTensorTensorType =>
          if res.size != 1 then
            Err(s"d_linalg.matmul: tensor form expects 1 result, got ${res.size}")
          else if res.head.typ != t then
            Err(s"d_linalg.matmul: tensor form result type must be ${t}, got ${res.head.typ}")
          else OK(this)
        case _: d_memref.dMemrefMemrefType =>
          if res.nonEmpty then
            Err(s"d_linalg.matmul: memref form expects 0 results, got ${res.size}")
          else OK(this)
        case _ => Err(s"d_linalg.matmul: unsupported output type ${out.typ}")

final case class Yield(
    args: Seq[Operand[Attribute]]
) extends DerivedOperation["d_linalg.yield"]
    with IsTerminator
    with NoMemoryEffect derives OpDefs

val dLinalgDialect = summonDialect[
  EmptyTuple,
  (Fill, Matmul, Yield),
]

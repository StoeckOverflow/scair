package scair.dialects.dTensor

import fastparse.*
import scair.Printer
import scair.dialects.builtin.*
import scair.ir.*
import scair.parse.*
import scair.parse.given
import scair.utils.*

object dTensorTypeUtil:

  def renderAttr(a: Attribute): String =
    val out = java.io.StringWriter()
    val printer = Printer(p = java.io.PrintWriter(out))
    printer.print(a)
    printer.flush()
    out.toString

  def checkParam(param: ValueAttribute): OK[Unit] =
    param match
      case va: ValueAttribute =>
        va.getVal().typ match
          case _: dTensorNatType => OK(())
          case other             =>
            Err(
              s"shape SSA parameter must have type !dtensor.nat, got ${renderAttr(other)}"
            )

  def elemOK(elem: TypeAttribute): Boolean =
    elem match
      case _: IntegerType => true
      case _: FloatType   => true
      case _              => false

  def asdTensor(t: dTensorType): dTensorTensorType =
    t match
      case dTensorVectorType(param, elem) =>
        dTensorTensorType(Seq(param), elem)
      case dTensorMatrixType(rows, cols, elem) =>
        dTensorTensorType(Seq(rows, cols), elem)
      case tt: dTensorTensorType =>
        tt

  def sameDims(
      lhs: Seq[ValueAttribute],
      rhs: Seq[ValueAttribute],
  ): Boolean =
    lhs.size == rhs.size && lhs.zip(rhs)
      .forall((l, r) => (l.getVal() eq r.getVal()))

  def checkSameTensorShapeAndElem(
      lhs: dTensorTensorType,
      rhs: dTensorTensorType,
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
        s"$opName: expected equal ranks for $lhsName/$rhsName, got ${lhs.params
            .size} and ${rhs.params.size}"
      )
    else if !sameDims(lhs.params, rhs.params) then
      Err(
        s"$opName: expected pairwise SSA-identical dims for $lhsName/$rhsName, got ${renderAttr(lhs)} and ${renderAttr(rhs)}"
      )
    else OK(())

  def checkTensorElementwise(
      lhs: dTensorTensorType,
      rhs: dTensorTensorType,
      res: dTensorTensorType,
      opName: String,
  ): OK[Unit] =
    checkSameTensorShapeAndElem(lhs, rhs, opName, "lhs", "rhs").flatMap(_ =>
      checkSameTensorShapeAndElem(lhs, res, opName, "lhs", "result")
    )

  def checkMatmul(
      lhs: dTensorTensorType,
      rhs: dTensorTensorType,
      res: dTensorTensorType,
  ): OK[Unit] =
    if lhs.params.size != 2 || rhs.params.size != 2 then
      Err(
        s"dtensor.matmul: expected rank-2 operands, got rank ${lhs.params
            .size} and ${rhs.params.size}"
      )
    else if res.params.size != 2 then
      Err(
        s"dtensor.matmul: expected rank-2 result, got rank ${res.params.size}"
      )
    else if lhs.elem != rhs.elem || lhs.elem != res.elem then
      Err(
        s"dtensor.matmul: expected equal element types for lhs/rhs/result, got ${renderAttr(lhs.elem)}, ${renderAttr(rhs.elem)}, ${renderAttr(res.elem)}"
      )
    else if lhs.params(1).getVal().ne(rhs.params(0).getVal()) then
      Err("dtensor.matmul: expected SSA-identical inner dims (lhs.k === rhs.k)")
    else if (lhs.params(0).getVal().ne(res.params(0).getVal())) ||
      (rhs.params(1).getVal().ne(res.params(1).getVal()))
    then
      Err(
        "dtensor.matmul: expected result dims to be outer dims (lhs.m, rhs.n)"
      )
    else OK(())

private def ValueAttributeP[$: P](using p: Parser): P[ValueAttribute] = P(
  operandNameP.flatMap(existingOperandP).map(v => ValueAttribute(v))
)

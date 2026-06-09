package scair.dialects.d_tensor

import scair.print.AssemblyPrinter
import scair.dialects.builtin.*
import scair.ir.*
import scair.utils.*

object DTensorTypeUtil:

  def renderAttr(a: Attribute): String =
    val out = java.io.StringWriter()
    val printer = AssemblyPrinter(p = java.io.PrintWriter(out))
    printer.print(a)
    printer.flush()
    out.toString

  def printDimParam(p: scair.print.Printer, param: DimParam): Unit =
    param match
      case v: ValueAttribute => p.print(v)
      case i: IntegerAttr    => p.print(i)

  private def resolveIndexBase(
      v: Value[Attribute],
      seen: Set[Value[Attribute]] = Set.empty,
  ): OK[Value[Attribute]] =
    if seen.contains(v) then
      Err("shape SSA parameter contains a cyclic !value<...> reference")
    else
      v.typ match
        case _: IndexType     => OK(v)
        case ValueRefType(ref) => resolveIndexBase(ref.getVal(), seen + v)
        case other =>
          Err(
            s"shape SSA parameter must have type index, got ${renderAttr(other)}"
          )

  def resolveIndexValue(v: Value[Attribute]): OK[Value[Attribute]] =
    resolveIndexBase(v)

  def checkParam(param: DimParam): OK[Unit] =
    param match
      case v: ValueAttribute => resolveIndexBase(v.getVal()).map(_ => ())
      case IntegerAttr(_, _: IndexType)   => OK(())
      case IntegerAttr(_, _: IntegerType) => OK(())

  def valueDim(param: DimParam, context: String): OK[Value[Attribute]] =
    param match
      case v: ValueAttribute => resolveIndexBase(v.getVal()).map(_ => v.getVal())
      case _                 => Err(s"$context: expected SSA dimension, got static dimension ${renderAttr(param)}")

  def elemOK(elem: TypeAttribute): Boolean =
    elem match
      case _: IntegerType => true
      case _: FloatType   => true
      case _              => false

  def asDTensor(t: DTensorType): DTensorTensorType =
    t match
      case DTensorVectorType(param, elem) =>
        DTensorTensorType(Seq(param), elem)
      case DTensorMatrixType(rows, cols, elem) =>
        DTensorTensorType(Seq(rows, cols), elem)
      case tt: DTensorTensorType =>
        tt

  def sameDims(
      lhs: Seq[DimParam],
      rhs: Seq[DimParam],
  ): Boolean =
    lhs.size == rhs.size && lhs.zip(rhs).forall {
      case (l: ValueAttribute, r: ValueAttribute) =>
        (resolveIndexBase(l.getVal()), resolveIndexBase(r.getVal())) match
          case (OK(lv), OK(rv)) => lv eq rv
          case _                => false
      case (l: IntegerAttr, r: IntegerAttr) =>
        l.value == r.value
      case _ => false
    }

  def checkSameTensorShapeAndElem(
      lhs: DTensorTensorType,
      rhs: DTensorTensorType,
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
      lhs: DTensorTensorType,
      rhs: DTensorTensorType,
      res: DTensorTensorType,
      opName: String,
  ): OK[Unit] =
    checkSameTensorShapeAndElem(lhs, rhs, opName, "lhs", "rhs").flatMap(_ =>
      checkSameTensorShapeAndElem(lhs, res, opName, "lhs", "result")
    )

  def checkMatmul(
      lhs: DTensorTensorType,
      rhs: DTensorTensorType,
      res: DTensorTensorType,
  ): OK[Unit] =
    if lhs.params.size != 2 || rhs.params.size != 2 then
      Err(
        s"d_tensor.matmul: expected rank-2 operands, got rank ${lhs.params
            .size} and ${rhs.params.size}"
      )
    else if res.params.size != 2 then
      Err(
        s"d_tensor.matmul: expected rank-2 result, got rank ${res.params.size}"
      )
    else if lhs.elem != rhs.elem || lhs.elem != res.elem then
      Err(
        s"d_tensor.matmul: expected equal element types for lhs/rhs/result, got ${renderAttr(lhs.elem)}, ${renderAttr(rhs.elem)}, ${renderAttr(res.elem)}"
      )
    else if !sameDims(Seq(lhs.params(1)), Seq(rhs.params(0))) then
      Err("d_tensor.matmul: expected SSA-identical inner dims (lhs.k === rhs.k)")
    else if !sameDims(Seq(lhs.params(0)), Seq(res.params(0))) ||
      !sameDims(Seq(rhs.params(1)), Seq(res.params(1)))
    then
      Err(
        "d_tensor.matmul: expected result dims to be outer dims (lhs.m, rhs.n)"
      )
    else OK(())

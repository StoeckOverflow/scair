package scair.dialects.dTensor

import fastparse.*
import scair.print.AssemblyPrinter
import scair.print.Printer
import scair.dialects.builtin.*
import scair.ir.*
import scair.parse.*
import scair.parse.given
import scair.utils.*

object dTensorTypeUtil:

  private def resolveNatBase(
      v: Value[Attribute],
      seen: Set[Value[Attribute]] = Set.empty,
  ): OK[Value[Attribute]] =
    if seen.contains(v) then
      Err("shape SSA parameter contains a cyclic !value<...> reference")
    else
      v.typ match
        case _: dTensorNatType => OK(v)
        case ValueRefType(ref) => resolveNatBase(ref.getVal(), seen + v)
        case other             =>
          Err(
            s"shape SSA parameter must have type !dtensor.nat, got ${renderAttr(other)}"
          )

  def resolveNatValue(v: Value[Attribute]): OK[Value[Attribute]] =
    resolveNatBase(v)

  def resolveNatFromIndexValue(v: Value[Attribute]): OK[Value[Attribute]] =
    v.typ match
      case _: IndexType =>
        v.owner match
          case Some(ShapeToIndex(nat, _)) => resolveNatBase(nat)
          case _                          =>
            val ownerName = v.owner match
              case Some(op: Operation) => op.name
              case Some(_: Block)      => "<block-arg>"
              case None                => "<unknown>"
            Err(
              s"index value does not carry dtensor shape provenance; expected producer `dtensor.shape.to_index`, got `$ownerName`"
            )
      case ValueRefType(ref) => resolveNatFromIndexValue(ref.getVal())
      case other             =>
        Err(
          s"expected index value for shape provenance resolution, got ${renderAttr(other)}"
        )

  def resolveNatProvenance(v: Value[Attribute]): OK[Value[Attribute]] =
    resolveNatValue(v) match
      case ok @ OK(_) => ok
      case _          => resolveNatFromIndexValue(v)

  def renderAttr(a: Attribute): String =
    val out = java.io.StringWriter()
    val printer = AssemblyPrinter(p = java.io.PrintWriter(out))
    printer.print(a)
    printer.flush()
    out.toString

  def checkParam(param: ValueAttribute): OK[Unit] =
    resolveNatBase(param.getVal()).map(_ => ())

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
    lhs.size == rhs.size && lhs.zip(rhs).forall((l, r) =>
      (resolveNatBase(l.getVal()), resolveNatBase(r.getVal())) match
        case (OK(lv), OK(rv)) => lv eq rv
        case _                => false
    )

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
    else if !sameDims(Seq(lhs.params(1)), Seq(rhs.params(0))) then
      Err("dtensor.matmul: expected SSA-identical inner dims (lhs.k === rhs.k)")
    else if !sameDims(Seq(lhs.params(0)), Seq(res.params(0))) ||
      !sameDims(Seq(rhs.params(1)), Seq(res.params(1)))
    then
      Err(
        "dtensor.matmul: expected result dims to be outer dims (lhs.m, rhs.n)"
      )
    else OK(())

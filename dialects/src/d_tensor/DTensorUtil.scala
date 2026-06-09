package scair.dialects.d_tensor

import scair.print.AssemblyPrinter
import scair.dialects.builtin.*
import scair.ir.*
import scair.utils.*

object DTensorTypeUtil:

  enum NatProductFactor:
    case Const(value: BigInt)
    case Atom(value: Value[Attribute])

  final case class NatProductFactors(factors: Seq[NatProductFactor])

  private def resolveSizeWitnessBase(
      v: Value[Attribute],
      seen: Set[Value[Attribute]] = Set.empty,
  ): OK[Value[Attribute]] =
    if seen.contains(v) then
      Err("shape SSA parameter contains a cyclic !value<...> reference")
    else
      v.typ match
        case _: DTensorSizeWitnessType => OK(v)
        case ValueRefType(ref) => resolveSizeWitnessBase(ref.getVal(), seen + v)
        case other             =>
          Err(
            s"shape SSA parameter must have type !d_tensor.size or !d_tensor.pos_size, got ${renderAttr(other)}"
          )

  def resolveSizeWitnessValue(v: Value[Attribute]): OK[Value[Attribute]] =
    resolveSizeWitnessBase(v)

  def resolveSizeWitnessFromIndexValue(v: Value[Attribute]): OK[Value[Attribute]] =
    v.typ match
      case ValueRefType(ref) => resolveSizeWitnessFromIndexValue(ref.getVal())
      case _: DTensorSizeWitnessType => resolveSizeWitnessBase(v)
      case other             =>
        Err(
          s"expected d_tensor size witness for shape provenance resolution, got ${renderAttr(other)}"
        )

  def resolveSizeWitnessProvenance(v: Value[Attribute]): OK[Value[Attribute]] =
    resolveSizeWitnessValue(v)

  def renderAttr(a: Attribute): String =
    val out = java.io.StringWriter()
    val printer = AssemblyPrinter(p = java.io.PrintWriter(out))
    printer.print(a)
    printer.flush()
    out.toString

  def checkParam(param: ValueAttribute): OK[Unit] =
    resolveSizeWitnessBase(param.getVal()).map(_ => ())

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
      lhs: Seq[ValueAttribute],
      rhs: Seq[ValueAttribute],
  ): Boolean =
    lhs.size == rhs.size && lhs.zip(rhs).forall((l, r) =>
      (resolveSizeWitnessBase(l.getVal()), resolveSizeWitnessBase(r.getVal())) match
        case (OK(lv), OK(rv)) => lv eq rv
        case _                => false
    )

  private def natConstValue(v: Value[Attribute]): Option[BigInt] =
    v.owner match
      case Some(SizeConstant(IntegerAttr(IntData(k), _), _)) => Some(k)
      case _                                             => None

  private def appendConstProduct(
      factors: Seq[NatProductFactor],
      value: BigInt,
  ): Seq[NatProductFactor] =
    if value == 1 then factors
    else
      factors.lastOption match
        case Some(NatProductFactor.Const(prev)) =>
          factors.dropRight(1) :+ NatProductFactor.Const(prev * value)
        case _ => factors :+ NatProductFactor.Const(value)

  def orderedSizeProductFactors(
      v: Value[Attribute]
  ): OK[NatProductFactors] =
    resolveSizeWitnessBase(v).flatMap(base =>
      natConstValue(base) match
        case Some(k) => OK(NatProductFactors(appendConstProduct(Seq.empty, k)))
        case None =>
          base.owner match
            case Some(SizeMul(lhs, rhs, _)) =>
              orderedSizeProductFactors(lhs).flatMap(lhsFactors =>
                orderedSizeProductFactors(rhs).map(rhsFactors =>
                  NatProductFactors(
                    (lhsFactors.factors ++ rhsFactors.factors).foldLeft(
                      Seq.empty[NatProductFactor]
                    ) {
                      case (acc, NatProductFactor.Const(k)) =>
                        appendConstProduct(acc, k)
                      case (acc, factor) => acc :+ factor
                    }
                  )
                )
              )
            case _ => OK(NatProductFactors(Seq(NatProductFactor.Atom(base))))
    )

  def sameOrderedSizeProduct(
      lhs: Value[Attribute],
      rhs: Seq[Value[Attribute]],
  ): OK[Boolean] =
    val rhsFactors = rhs.foldLeft[OK[NatProductFactors]](
      OK(NatProductFactors(Seq.empty))
    ) { case (acc, dim) =>
      acc.flatMap(factors =>
        orderedSizeProductFactors(dim).map(dimFactors =>
          NatProductFactors(
            (factors.factors ++ dimFactors.factors).foldLeft(
              Seq.empty[NatProductFactor]
            ) {
              case (merged, NatProductFactor.Const(k)) =>
                appendConstProduct(merged, k)
              case (merged, factor) => merged :+ factor
            }
          )
        )
      )
    }

    orderedSizeProductFactors(lhs).flatMap(lhsFactors =>
      rhsFactors.map(rhsFactors =>
        lhsFactors.factors.size == rhsFactors.factors.size &&
          lhsFactors.factors.zip(rhsFactors.factors).forall {
            case (NatProductFactor.Const(l), NatProductFactor.Const(r)) =>
              l == r
            case (NatProductFactor.Atom(l), NatProductFactor.Atom(r)) =>
              l eq r
            case _ => false
          }
      )
    )

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

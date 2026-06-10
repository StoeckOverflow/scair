package scair.dialects.d_memref

import fastparse.*
import scair.print.Printer
import scair.clair.*
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.d_tensor.*
import scair.ir.*
import scair.parse.*
import scair.parse.given
import scair.utils.*

type DimParam = ValueAttribute | IntegerAttr
type LayoutParam = ValueAttribute | IntegerAttr

sealed trait DMemrefType extends ParametrizedAttribute, TypeAttribute

object DMemrefTypeUtil:

  def renderAttr(a: Attribute): String = DTensorTypeUtil.renderAttr(a)

  private def staticIntegerValue(param: ValueAttribute | IntegerAttr): Option[BigInt] =
    param match
      case IntegerAttr(IntData(value), _: IndexType | _: IntegerType) =>
        Some(value)
      case _ => None

  def renderDimParam(param: DimParam): String =
    param match
      case v: ValueAttribute => renderAttr(v)
      case i: IntegerAttr    => renderAttr(i)

  def printDimParam(p: Printer, param: DimParam): Unit =
    param match
      case v: ValueAttribute => p.print(v)
      case i: IntegerAttr    => p.print(i)

  def dimParamAttribute(param: DimParam): Attribute =
    param match
      case v: ValueAttribute => v
      case i: IntegerAttr    => i

  def checkParam(param: DimParam): OK[Unit] =
    param match
      case v: ValueAttribute => DTensorTypeUtil.checkParam(v)
      case IntegerAttr(IntData(value), _: IndexType | _: IntegerType) =>
        if value < 0 then
          Err(s"d_memref: expected non-negative static dimension, got $value")
        else OK(())

  def elemOK(elem: TypeAttribute): Boolean = DTensorTypeUtil.elemOK(elem)

  def renderLayoutParam(param: LayoutParam): String =
    param match
      case v: ValueAttribute => renderAttr(v)
      case i: IntegerAttr    => renderAttr(i)

  def printLayoutParam(p: Printer, param: LayoutParam): Unit =
    param match
      case v: ValueAttribute => p.print(v)
      case i: IntegerAttr    => p.print(i)

  def layoutParamAttribute(param: LayoutParam): Attribute =
    param match
      case v: ValueAttribute => v
      case i: IntegerAttr    => i

  def checkLayoutParam(param: LayoutParam): OK[Unit] =
    param match
      case v: ValueAttribute =>
        v.getVal().typ match
          case _: IndexType      => OK(())
          case _: IntegerType    => OK(())
          case _: DTensorNatLikeType => DTensorTypeUtil.resolveNatValue(v.getVal()).map(_ => ())
          case ValueRefType(ref) => checkLayoutParam(ValueAttribute(ref.getVal()))
          case other =>
            Err(
              s"layout SSA parameter must have type index, integer, !d_tensor.nat, !d_tensor.posnat, or !value<...>, got ${renderAttr(other)}"
            )
      case IntegerAttr(_, _: IndexType)   => OK(())
      case IntegerAttr(_, _: IntegerType) => OK(())
      case other =>
        Err(
          s"d_memref: expected layout parameter to be an index/integer attribute or supported SSA value, got ${renderAttr(layoutParamAttribute(other))}"
        )

  def checkOffsetParam(param: LayoutParam): OK[Unit] =
    checkLayoutParam(param).flatMap(_ =>
      staticIntegerValue(param) match
        case Some(value) if value < 0 =>
          Err(s"d_memref.memref: expected non-negative static offset, got $value")
        case _ => OK(())
    )

  def checkStrideParam(param: LayoutParam): OK[Unit] =
    checkLayoutParam(param).flatMap(_ =>
      staticIntegerValue(param) match
        case Some(value) if value <= 0 =>
          Err(s"d_memref.memref: expected positive static stride, got $value")
        case _ => OK(())
    )

  def staticLayoutValue(param: LayoutParam): Option[BigInt] =
    staticIntegerValue(param)

  def sameLayoutParam(lhs: LayoutParam, rhs: LayoutParam): Boolean =
    (lhs, rhs) match
      case (l: ValueAttribute, r: ValueAttribute) =>
        l.getVal() eq r.getVal()
      case (l: IntegerAttr, r: IntegerAttr) =>
        l == r
      case _ => false

  def sameLayout(
      lhsOffset: Option[LayoutParam],
      lhsStrides: Option[Seq[LayoutParam]],
      rhsOffset: Option[LayoutParam],
      rhsStrides: Option[Seq[LayoutParam]],
  ): Boolean =
    (lhsOffset, lhsStrides, rhsOffset, rhsStrides) match
      case (None, None, None, None) => true
      case (Some(lo), Some(ls), Some(ro), Some(rs)) =>
        sameLayoutParam(lo, ro) &&
        ls.size == rs.size &&
        ls.zip(rs).forall((l, r) => sameLayoutParam(l, r))
      case _ => false

  def asMemref(t: DMemrefType): DMemrefMemrefType =
    t match
      case DMemrefVectorType(param, elem) =>
        DMemrefMemrefType(Seq(param), elem)
      case DMemrefMatrixType(rows, cols, elem) =>
        DMemrefMemrefType(Seq(rows, cols), elem)
      case mt: DMemrefMemrefType =>
        mt

  def sameDim(lhs: DimParam, rhs: DimParam): Boolean =
    (lhs, rhs) match
      case (l: ValueAttribute, r: ValueAttribute) =>
        DTensorTypeUtil.sameDims(Seq(l), Seq(r))
      case (l: IntegerAttr, r: IntegerAttr) =>
        l.value == r.value
      case _ => false

  def sameDims(lhs: Seq[DimParam], rhs: Seq[DimParam]): Boolean =
    lhs.size == rhs.size && lhs.zip(rhs).forall((l, r) => sameDim(l, r))

private def parseDimParam[$: P](using Parser): P[DimParam] =
  P(
    ValueAttributeP.map(v => v: DimParam) |
      attrOfP[IntegerAttr].map(i => i: DimParam) |
      decimalLiteralP.map(v => IntegerAttr(IntData(v), IndexType()): DimParam)
  )

private def parseLayoutParam[$: P](using Parser): P[LayoutParam] =
  P(
    ValueAttributeP.map(v => v: LayoutParam) |
      attrOfP[IntegerAttr].map(i => i: LayoutParam) |
      decimalLiteralP.map(v => IntegerAttr(IntData(v), IndexType()): LayoutParam)
  )

final case class DMemrefVectorType(param: DimParam, elem: TypeAttribute)
    extends DMemrefType:
  override def name: String = "d_memref.vector"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(param, elem)
  override def rebuild(parameters: Seq[Attribute | Seq[Attribute]]): Attribute =
    DMemrefVectorType(
      parameters(0).asInstanceOf[DimParam],
      parameters(1).asInstanceOf[TypeAttribute],
    )

  override def customVerify(): OK[Unit] =
    DMemrefTypeUtil.checkParam(param).flatMap(_ =>
      if DMemrefTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_memref.vector element type `${DMemrefTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[DMemrefVectorType]:
  override def name: String = "d_memref.vector"

  override def parse[$: P](using Parser): P[DMemrefVectorType] =
    P("<" ~ parseDimParam ~ "," ~ typeP ~ ">").map((param, elem) =>
      DMemrefVectorType(param, elem.asInstanceOf[TypeAttribute])
    )

final case class DMemrefMatrixType(
    rows: DimParam,
    cols: DimParam,
    elem: TypeAttribute,
) extends DMemrefType:
  override def name: String = "d_memref.matrix"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(rows, cols, elem)
  override def rebuild(parameters: Seq[Attribute | Seq[Attribute]]): Attribute =
    DMemrefMatrixType(
      parameters(0).asInstanceOf[DimParam],
      parameters(1).asInstanceOf[DimParam],
      parameters(2).asInstanceOf[TypeAttribute],
    )

  override def customVerify(): OK[Unit] =
    DMemrefTypeUtil.checkParam(rows).flatMap(_ =>
      DMemrefTypeUtil.checkParam(cols)
    ).flatMap(_ =>
      if DMemrefTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_memref.matrix element type `${DMemrefTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[DMemrefMatrixType]:
  override def name: String = "d_memref.matrix"

  override def parse[$: P](using Parser): P[DMemrefMatrixType] =
    P("<" ~ parseDimParam ~ "," ~ parseDimParam ~ "," ~ typeP ~ ">")
      .map((rows, cols, elem) =>
        DMemrefMatrixType(rows, cols, elem.asInstanceOf[TypeAttribute])
      )

final case class DMemrefMemrefType(
    params: Seq[DimParam],
    elem: TypeAttribute,
    offset: Option[LayoutParam] = None,
    strides: Option[Seq[LayoutParam]] = None,
) extends DMemrefType:
  override def name: String = "d_memref.memref"

  override def parameters: Seq[Attribute | Seq[Attribute]] =
    Seq(params, elem) ++
      offset.map(DMemrefTypeUtil.layoutParamAttribute) ++
      strides.toSeq.map(_.map(DMemrefTypeUtil.layoutParamAttribute))

  override def rebuild(parameters: Seq[Attribute | Seq[Attribute]]): Attribute =
    val rebuiltOffset =
      if offset.isDefined then Some(parameters(2).asInstanceOf[LayoutParam])
      else None
    val rebuiltStrides =
      if strides.isDefined then
        Some(parameters(3).asInstanceOf[Seq[LayoutParam]])
      else None
    DMemrefMemrefType(
      parameters(0).asInstanceOf[Seq[DimParam]],
      parameters(1).asInstanceOf[TypeAttribute],
      rebuiltOffset,
      rebuiltStrides,
    )

  override def printParameters(p: Printer): Unit =
    given indentLevel: Int = 0
    p.print("<[")
    p.printListF(params, param => DMemrefTypeUtil.printDimParam(p, param), sep = ", ")
    p.print("], ", elem)
    (offset, strides) match
      case (Some(off), Some(ss)) =>
        p.print(", offset: ")
        DMemrefTypeUtil.printLayoutParam(p, off)
        p.print(", strides: [")
        p.printListF(ss, s => DMemrefTypeUtil.printLayoutParam(p, s), sep = ", ")
        p.print("]")
      case _ => ()
    p.print(">")

  override def customVerify(): OK[Unit] =
    params.foldLeft[OK[Unit]](OK(()))((acc, p) =>
      acc.flatMap(_ => DMemrefTypeUtil.checkParam(p))
    ).flatMap(_ =>
      if DMemrefTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_memref element type `${DMemrefTypeUtil.renderAttr(elem)}`"
        )
    ).flatMap(_ =>
      (offset, strides) match
        case (None, None) => OK(())
        case (Some(off), Some(ss)) =>
          if ss.size != params.size then
            Err(
              s"d_memref.memref: expected ${params.size} strides for rank ${params.size}, got ${ss.size}"
            )
          else
            DMemrefTypeUtil.checkOffsetParam(off).flatMap(_ =>
              ss.foldLeft[OK[Unit]](OK(()))((acc, s) =>
                acc.flatMap(_ => DMemrefTypeUtil.checkStrideParam(s))
              )
            )
        case _ =>
          Err("d_memref.memref: offset and strides must be specified together")
    )

given AttributeCompanion[DMemrefMemrefType]:
  override def name: String = "d_memref.memref"

  override def parse[$: P](using Parser): P[DMemrefMemrefType] =
    P(
      "<" ~ "[" ~ parseDimParam.rep(sep = ",") ~ "]" ~ "," ~ typeP ~
        ("," ~ "offset:" ~ parseLayoutParam ~ "," ~ "strides:" ~ "[" ~
          parseLayoutParam.rep(sep = ",") ~ "]").? ~ ">"
    ).map((params, elem, layoutOpt) =>
      val (offset, strides) = layoutOpt match
        case Some((off, ss)) => (Some(off), Some(ss))
        case None            => (None, None)
      DMemrefMemrefType(
        params,
        elem.asInstanceOf[TypeAttribute],
        offset,
        strides,
      )
    )

private def parseIndexOperands[$: P](names: Seq[String])(using
    p: Parser
): P[Seq[Operand[IndexType]]] =
  names.foldLeft(Pass(Seq.empty[Operand[IndexType]]))((acc, n) =>
    (acc ~ operandP(n, IndexType())).map(_ :+ _)
  )

final case class Alloc(
    res: Result[DMemrefMemrefType]
) extends DerivedOperation["d_memref.alloc"]
    derives OpDefs:

  override def customPrint(printer: Printer): Unit =
    printer.print(name, " : () -> ", res.typ)

given OperationCustomParser[Alloc]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Alloc] =
    P(":" ~ "(" ~ ")" ~ "->" ~ typeOfP[DMemrefMemrefType]).flatMap(typ =>
      resultP(resNames.head, typ).map(Alloc(_))
    )

final case class Dealloc(
    memref: Operand[DMemrefMemrefType]
) extends DerivedOperation["d_memref.dealloc"]
    derives OpDefs:

  override def customPrint(printer: Printer): Unit =
    printer.print(name, " ", memref, " : ", memref.typ)

given OperationCustomParser[Dealloc]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Dealloc] =
    P(operandNameP ~ ":" ~ typeOfP[DMemrefMemrefType]).flatMap((mName, mTyp) =>
      operandP(mName, mTyp).map(Dealloc(_))
    )

final case class Dim(
    memref: Operand[DMemrefMemrefType],
    axis: Operand[IndexType],
    res: Result[IndexType],
) extends DerivedOperation["d_memref.dim"]
    with NoMemoryEffect derives OpDefs:

  private def constantAxisValue: Option[BigInt] =
    axis.owner match
      case Some(arith.Constant(IntegerAttr(IntData(v), _: IndexType), _)) =>
        Some(v)
      case _ =>
        None

  override def customVerify(): OK[Operation] =
    if res.typ != IndexType() then
      Err(s"d_memref.dim: expected result type index, got ${res.typ}")
    else
      constantAxisValue match
        case Some(v) if v < 0 || v >= memref.typ.params.size =>
          Err(
            s"d_memref.dim: constant axis $v out of bounds for rank ${memref.typ.params.size}"
          )
        case _ => OK(this)

  override def customPrint(printer: Printer): Unit =
    printer.print(
      name,
      " ",
      memref,
      ", ",
      axis,
      " : ",
      memref.typ,
      " -> ",
      res.typ,
    )

given OperationCustomParser[Dim]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Dim] =
    P(
      operandNameP ~ "," ~ operandNameP ~ ":" ~ typeOfP[DMemrefMemrefType] ~
        "->" ~ typeOfP[IndexType]
    ).flatMap((mName, axisName, mTyp, rTyp) =>
      operandP(mName, mTyp).flatMap(m =>
        operandP(axisName, IndexType()).flatMap(axis =>
          resultP(resNames.head, rTyp).map(r => Dim(m, axis, r))
        )
      )
    )

final case class DimExact(
    memref: Operand[DMemrefMemrefType],
    axis: IntegerAttr,
    res: Result[ValueRefType],
) extends DerivedOperation["d_memref.dim_exact"]
    with NoMemoryEffect derives OpDefs:

  private def selectedDimValue: OK[Value[Attribute]] =
    val idx = axis.value.value
    val rank = BigInt(memref.typ.params.size)
    if idx < 0 || idx >= rank then
      Err(s"d_memref.dim_exact: axis $idx out of bounds for rank ${memref.typ.params.size}")
    else
      memref.typ.params(idx.toInt) match
        case v: ValueAttribute => OK(v.getVal())
        case _ =>
          Err(
            "d_memref.dim_exact: expected selected embedded dim to be SSA-backed, got a literal dimension"
          )

  override def customVerify(): OK[Operation] =
    if axis.typ != I32 then
      Err(s"d_memref.dim_exact: expected i32 axis attribute, got ${axis.typ}")
    else
      selectedDimValue.flatMap(sel =>
        if res.typ.ref.getVal() eq sel then
          DTensorTypeUtil.resolveNatValue(res.typ.ref.getVal()).map(_ => this)
        else
          Err(
            "d_memref.dim_exact: expected result !value<...> to reference the selected embedded dim"
          )
      )

  override def customPrint(printer: Printer): Unit =
    printer.print(
      name,
      " ",
      memref,
      " {axis = ",
      axis,
      "} : ",
      memref.typ,
      " -> ",
      res.typ,
    )

given OperationCustomParser[DimExact]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[DimExact] =
    P(
      operandNameP ~ "{" ~ "axis" ~ "=" ~ attrOfP[IntegerAttr] ~ "}" ~ ":" ~
        typeOfP[DMemrefMemrefType] ~ "->" ~ typeOfP[ValueRefType]
    ).flatMap((mName, axis, mTyp, rTyp) =>
      operandP(mName, mTyp).flatMap(m => resultP(resNames.head, rTyp).map(r =>
        DimExact(m, axis, r)
      ))
    )

final case class Load(
    memref: Operand[DMemrefMemrefType],
    indices: Seq[Operand[IndexType]],
    res: Result[TypeAttribute],
) extends DerivedOperation["d_memref.load"]
    derives OpDefs:

  override def customVerify(): OK[Operation] =
    if indices.size != memref.typ.params.size then
      Err(
        s"d_memref.load: expected ${memref.typ.params.size} indices, got ${indices.size}"
      )
    else if res.typ != memref.typ.elem then
      Err(
        s"d_memref.load: expected result type ${memref.typ.elem}, got ${res.typ}"
      )
    else OK(this)

  override def customPrint(printer: Printer): Unit =
    printer.print(name, " ", memref, "[")
    printer.printList(indices)
    printer.print("] : ", memref.typ, " -> ", res.typ)

given OperationCustomParser[Load]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Load] =
    P(
      operandNameP ~ "[" ~ operandNameP.rep(sep = ",") ~ "]" ~ ":" ~
        typeOfP[DMemrefMemrefType] ~ "->" ~ typeP
    ).flatMap((mName, idxNames, mTyp, rTyp) =>
      operandP(mName, mTyp).flatMap(m =>
        parseIndexOperands(idxNames).flatMap(idxs =>
          resultP(resNames.head, rTyp.asInstanceOf[TypeAttribute]).map(r =>
            Load(m, idxs, r)
          )
        )
      )
    )

final case class ExtractStridedMetadata(
    source: Operand[DMemrefMemrefType],
    _results: Seq[Result[Attribute]],
) extends DerivedOperation["d_memref.extract_strided_metadata"]
    derives OpDefs:

  override def customVerify(): OK[Operation] =
    val expected = ExtractStridedMetadata.resultTypesOf(source.typ)
    if _results.size != expected.size then
      Err(
        s"d_memref.extract_strided_metadata: expected ${expected.size} results for rank ${source.typ.params.size}, got ${_results.size}"
      )
    else
      _results.zip(expected).zipWithIndex.foldLeft[OK[Unit]](OK(())) {
        case (acc, ((result, expectedType), idx)) =>
          acc.flatMap(_ =>
            if result.typ == expectedType then OK(())
            else
              Err(
                s"d_memref.extract_strided_metadata: result $idx expected type ${DMemrefTypeUtil.renderAttr(expectedType)}, got ${DMemrefTypeUtil.renderAttr(result.typ)}"
              )
          )
      }.map(_ => this)

object ExtractStridedMetadata:
  def baseTypeOf(srcType: DMemrefMemrefType): DMemrefMemrefType =
    DMemrefMemrefType(Seq.empty, srcType.elem)

  def resultTypesOf(srcType: DMemrefMemrefType): Seq[Attribute] =
    Seq(baseTypeOf(srcType), IndexType()) ++ Seq.fill(srcType.params.size * 2)(IndexType())

  def build(source: Operand[DMemrefMemrefType]): ExtractStridedMetadata =
    ExtractStridedMetadata(source, resultTypesOf(source.typ).map(Result(_)))

final case class Store(
    value: Operand[TypeAttribute],
    memref: Operand[DMemrefMemrefType],
    indices: Seq[Operand[IndexType]],
) extends DerivedOperation["d_memref.store"]
    derives OpDefs:

  override def customVerify(): OK[Operation] =
    if indices.size != memref.typ.params.size then
      Err(
        s"d_memref.store: expected ${memref.typ.params.size} indices, got ${indices.size}"
      )
    else if value.typ != memref.typ.elem then
      Err(
        s"d_memref.store: expected stored value type ${memref.typ.elem}, got ${value.typ}"
      )
    else OK(this)

  override def customPrint(printer: Printer): Unit =
    printer.print(name, " ", value, ", ", memref, "[")
    printer.printList(indices)
    printer.print("] : ", value.typ, ", ", memref.typ)

given OperationCustomParser[Store]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Store] =
    P(
      operandNameP ~ "," ~ operandNameP ~ "[" ~ operandNameP.rep(sep = ",") ~
        "]" ~ ":" ~ typeP ~ "," ~ typeOfP[DMemrefMemrefType]
    ).flatMap((vName, mName, idxNames, vTyp, mTyp) =>
      operandP(vName, vTyp.asInstanceOf[TypeAttribute]).flatMap(v =>
        operandP(mName, mTyp).flatMap(m =>
          parseIndexOperands(idxNames).map(idxs => Store(v, m, idxs))
        )
      )
    )

final case class Cast(
    src: Operand[DMemrefMemrefType],
    res: Result[DMemrefMemrefType],
) extends DerivedOperation["d_memref.cast"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if src.typ.elem != res.typ.elem then
      Err(
        s"d_memref.cast: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if src.typ.params.size != res.typ.params.size then
      Err(
        s"d_memref.cast: expected equal ranks, got ${src.typ.params.size} and ${res.typ.params.size}"
      )
    else if !DMemrefTypeUtil.sameDims(src.typ.params, res.typ.params) then
      Err("d_memref.cast: expected pairwise SSA-identical dims")
    else if !DMemrefTypeUtil.sameLayout(
        src.typ.offset,
        src.typ.strides,
        res.typ.offset,
        res.typ.strides,
      )
    then Err("d_memref.cast: expected identical layout metadata")
    else OK(this)

  override def customPrint(printer: Printer): Unit =
    printer.print(name, " ", src, " : ", src.typ, " -> ", res.typ)

given OperationCustomParser[Cast]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Cast] =
    P(operandNameP ~ ":" ~ typeOfP[DMemrefMemrefType] ~ "->" ~ typeOfP[
      DMemrefMemrefType
    ]).flatMap((srcName, srcTyp, resTyp) =>
      operandP(srcName, srcTyp).flatMap(src =>
        resultP(resNames.head, resTyp).map(res => Cast(src, res))
      )
    )

final case class Subview(
    src: Operand[DMemrefMemrefType],
    offsets: Seq[Operand[IndexType]],
    sizes: Seq[Operand[IndexType]],
    strides: Seq[Operand[IndexType]],
    res: Result[DMemrefMemrefType],
) extends DerivedOperation["d_memref.subview"]
    derives OpDefs:

  private def sameDimAsSizeOperand(
      dim: DimParam,
      size: Operand[IndexType],
  ): Boolean =
    dim match
      case d: ValueAttribute =>
        val dimNat = DTensorTypeUtil.resolveNatValue(d.getVal())
        val sizeNat = DTensorTypeUtil.resolveNatFromIndexValue(size)
        (dimNat, sizeNat) match
          case (OK(lhs), OK(rhs)) => lhs eq rhs
          case _                  =>
            (d.getVal().owner, size.owner) match
              case (
                    Some(NatConst(IntegerAttr(IntData(lhs), _), _)),
                    Some(arith.Constant(IntegerAttr(IntData(rhs), _: IndexType), _)),
                  ) => lhs == rhs
              case _ => false
      case IntegerAttr(IntData(lhs), _: IndexType | _: IntegerType) =>
        size.owner match
          case Some(arith.Constant(IntegerAttr(IntData(rhs), _: IndexType), _)) =>
            lhs == rhs
          case _ => false

  private def firstSizeProvenanceMismatch: Option[Int] =
    res.typ.params.zip(sizes).zipWithIndex.collectFirst {
      case ((d, s), axis) if !sameDimAsSizeOperand(d, s) => axis
    }

  private def constantIndexValue(value: Operand[IndexType]): Option[BigInt] =
    value.owner match
      case Some(arith.Constant(IntegerAttr(IntData(v), _: IndexType), _)) =>
        Some(v)
      case _ => None

  private def allConstantIndexValues(values: Seq[Operand[IndexType]]): Option[Seq[BigInt]] =
    val constants = values.map(constantIndexValue)
    if constants.forall(_.isDefined) then Some(constants.flatten) else None

  private def staticDimValue(dim: DimParam): Option[BigInt] =
    dim match
      case IntegerAttr(IntData(value), _: IndexType | _: IntegerType) =>
        Some(value)
      case v: ValueAttribute =>
        DTensorTypeUtil.resolveNatValue(v.getVal()) match
          case OK(base) =>
            base.owner match
              case Some(NatConst(IntegerAttr(IntData(value), _), _)) => Some(value)
              case _                                                 => None
          case _ => None

  private def allStaticDimValues(dims: Seq[DimParam]): Option[Seq[BigInt]] =
    val constants = dims.map(staticDimValue)
    if constants.forall(_.isDefined) then Some(constants.flatten) else None

  private def verifyStaticSubviewBounds(): OK[Unit] =
    val staticContract =
      for
        dimVals <- allStaticDimValues(src.typ.params)
        offsetVals <- allConstantIndexValues(offsets)
        sizeVals <- allConstantIndexValues(sizes)
        strideVals <- allConstantIndexValues(strides)
      yield (dimVals, offsetVals, sizeVals, strideVals)

    staticContract match
      case None => OK(())
      case Some((dimVals, offsetVals, sizeVals, strideVals)) =>
        dimVals.indices.foldLeft[OK[Unit]](OK(())) { case (acc, axis) =>
          acc.flatMap(_ =>
            val dim = dimVals(axis)
            val offset = offsetVals(axis)
            val size = sizeVals(axis)
            val stride = strideVals(axis)
            if offset < 0 then
              Err(
                s"d_memref.subview: expected non-negative static offset at axis $axis, got $offset"
              )
            else if size < 0 then
              Err(
                s"d_memref.subview: expected non-negative static size at axis $axis, got $size"
              )
            else if stride <= 0 then
              Err(
                s"d_memref.subview: expected positive static stride at axis $axis, got $stride"
              )
            else
              val inBounds =
                if size == 0 then offset <= dim
                else offset + (size - 1) * stride < dim
              if inBounds then OK(())
              else
                Err(
                  s"d_memref.subview: static slice at axis $axis is out of bounds for dimension $dim (offset $offset, size $size, stride $stride)"
                )
          )
        }

  private def verifyExpectedStaticLayout(
      expectedOffset: BigInt,
      expectedStrides: Seq[BigInt],
      resOffset: LayoutParam,
      resStrides: Seq[LayoutParam],
  ): OK[Unit] =
    DMemrefTypeUtil.staticLayoutValue(resOffset) match
      case Some(value) if value != expectedOffset =>
        Err(
          s"d_memref.subview: result offset mismatch; expected $expectedOffset, got $value"
        )
      case None =>
        Err(
          s"d_memref.subview: expected statically derivable result offset $expectedOffset"
        )
      case _ =>
        expectedStrides.zip(resStrides).zipWithIndex.foldLeft[OK[Unit]](OK(())) {
          case (acc, ((expected, actualParam), axis)) =>
            acc.flatMap(_ =>
              DMemrefTypeUtil.staticLayoutValue(actualParam) match
                case Some(actual) if actual != expected =>
                  Err(
                    s"d_memref.subview: result stride mismatch at axis $axis; expected $expected, got $actual"
                  )
                case None =>
                  Err(
                    s"d_memref.subview: expected statically derivable result stride $expected at axis $axis"
                  )
                case _ => OK(())
            )
        }

  // Restricted verified subset: an explicit subview result layout is verified
  // only when it is statically derivable from the source layout and static
  // subview operands, or when it is the exact same dynamic layout for an
  // identity offset/stride slice. General symbolic layout arithmetic is left to
  // explicit witnesses or later lowering validation.
  private def verifySubviewLayout(): OK[Unit] =
    (res.typ.offset, res.typ.strides) match
      case (None, None) => OK(())
      case (Some(resOffset), Some(resStrides)) =>
        (src.typ.offset, src.typ.strides) match
          case (None, None) =>
            Err(
              "d_memref.subview: explicit result layout requires explicit source layout. Use d_memref.reinterpret_cast for metadata-only layout changes"
            )
          case (Some(srcOffset), Some(srcStrides)) =>
            if resStrides.size != srcStrides.size then
              Err(
                s"d_memref.subview: expected ${srcStrides.size} result strides, got ${resStrides.size}"
              )
            else
              (allConstantIndexValues(offsets), allConstantIndexValues(strides)) match
                case (Some(offsetVals), Some(strideVals))
                    if offsetVals.forall(_ == 0) && strideVals.forall(_ == 1) &&
                      DMemrefTypeUtil.sameLayout(
                        Some(srcOffset),
                        Some(srcStrides),
                        Some(resOffset),
                        Some(resStrides),
                      ) =>
                  OK(())
                case (Some(offsetVals), Some(strideVals)) =>
                  val staticSource =
                    for
                      off <- DMemrefTypeUtil.staticLayoutValue(srcOffset)
                      ss <- Option(srcStrides.map(DMemrefTypeUtil.staticLayoutValue))
                        .filter(_.forall(_.isDefined))
                        .map(_.flatten)
                    yield (off, ss)
                  staticSource match
                    case Some((srcOffValue, srcStrideValues)) =>
                      val expectedOffset =
                        srcOffValue + offsetVals.zip(srcStrideValues).map((off, stride) =>
                          off * stride
                        ).sum
                      val expectedStrides =
                        srcStrideValues.zip(strideVals).map((srcStride, subStride) =>
                          srcStride * subStride
                        )
                      verifyExpectedStaticLayout(
                        expectedOffset,
                        expectedStrides,
                        resOffset,
                        resStrides,
                      )
                    case None =>
                      Err(
                        "d_memref.subview: explicit result layout is outside the restricted verified subset unless it is statically derivable or an identity dynamic slice; use d_memref.reinterpret_cast for metadata-only layout changes"
                      )
                case _ =>
                  Err(
                    "d_memref.subview: explicit result layout is outside the restricted verified subset unless it is statically derivable or an identity dynamic slice; use d_memref.reinterpret_cast for metadata-only layout changes"
                  )
          case _ =>
            Err(
              "d_memref.subview: explicit source layout must specify offset and strides together"
            )
      case _ =>
        Err(
          "d_memref.subview: explicit result layout must specify offset and strides together"
        )

  override def customVerify(): OK[Operation] =
    val srcRank = src.typ.params.size
    val resRank = res.typ.params.size
    if srcRank != resRank then
      Err(s"d_memref.subview: expected equal source/result rank, got $srcRank and $resRank")
    else if offsets.size != srcRank then
      Err(s"d_memref.subview: expected $srcRank offsets, got ${offsets.size}")
    else if sizes.size != srcRank then
      Err(s"d_memref.subview: expected $srcRank sizes, got ${sizes.size}")
    else if strides.size != srcRank then
      Err(s"d_memref.subview: expected $srcRank strides, got ${strides.size}")
    else if src.typ.elem != res.typ.elem then
      Err(
        s"d_memref.subview: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else
      verifyStaticSubviewBounds().flatMap(_ =>
        firstSizeProvenanceMismatch match
          case Some(axis) =>
            Err(
              s"d_memref.subview: size provenance mismatch at axis $axis; expected result dim to match size operand via d_tensor.shape.to_index"
            )
          case None =>
            verifySubviewLayout().map(_ => this)
      )

  override def customPrint(printer: Printer): Unit =
    printer.print(name, " ", src, "[")
    printer.printList(offsets)
    printer.print("][")
    printer.printList(sizes)
    printer.print("][")
    printer.printList(strides)
    printer.print("] : ", src.typ, " -> ", res.typ)

given OperationCustomParser[Subview]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Subview] =
    P(
      operandNameP ~ "[" ~ operandNameP.rep(sep = ",") ~ "]" ~ "[" ~
        operandNameP.rep(sep = ",") ~ "]" ~ "[" ~ operandNameP.rep(sep = ",") ~
        "]" ~ ":" ~ typeOfP[DMemrefMemrefType] ~ "->" ~ typeOfP[DMemrefMemrefType]
    ).flatMap((srcName, offNames, sizeNames, strideNames, srcTyp, resTyp) =>
      operandP(srcName, srcTyp).flatMap(src =>
        parseIndexOperands(offNames).flatMap(offsets =>
          parseIndexOperands(sizeNames).flatMap(sizes =>
            parseIndexOperands(strideNames).flatMap(strides =>
              resultP(resNames.head, resTyp).map(res =>
                Subview(src, offsets, sizes, strides, res)
              )
            )
          )
        )
      )
    )

final case class ReinterpretCast(
    src: Operand[DMemrefMemrefType],
    res: Result[DMemrefMemrefType],
) extends DerivedOperation["d_memref.reinterpret_cast"]
    with NoMemoryEffect
    derives OpDefs:

  override def customVerify(): OK[Operation] =
    if src.typ.elem != res.typ.elem then
      Err(
        s"d_memref.reinterpret_cast: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if res.typ.offset.isEmpty || res.typ.strides.isEmpty then
      Err(
        "d_memref.reinterpret_cast: expected destination type to encode offset and strides"
      )
    else
      OK(this)

  override def customPrint(printer: Printer): Unit =
    printer.print(name, " ", src, "\n")
    printer.withIndent(printer.print(": ", src.typ, " to ", res.typ))

given OperationCustomParser[ReinterpretCast]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[ReinterpretCast] =
    P(operandNameP ~ ":" ~ typeOfP[DMemrefMemrefType] ~ "to" ~ typeOfP[DMemrefMemrefType])
      .flatMap((srcName, srcTyp, resTyp) =>
        operandP(srcName, srcTyp).flatMap(src =>
          resultP(resNames.head, resTyp).map(res => ReinterpretCast(src, res))
        )
      )


val DMemrefDialect = summonDialect[
  (DMemrefVectorType, DMemrefMatrixType, DMemrefMemrefType),
  (
      Alloc,
      Dealloc,
      Dim,
      DimExact,
      Load,
      ExtractStridedMetadata,
      Store,
      Cast,
      Subview,
      ReinterpretCast,
  ),
]

package scair.dialects.d_memref

import fastparse.*
import scair.Printer
import scair.clair.macros.*
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.dialects.llvm
import scair.ir.*
import scair.parse.*
import scair.parse.given
import scair.utils.*

type DimParam = ValueAttribute
type LayoutParam = ValueAttribute | IntegerAttr

sealed trait dMemrefType extends ParametrizedAttribute, TypeAttribute

object dMemrefTypeUtil:

  def renderAttr(a: Attribute): String = dTensorTypeUtil.renderAttr(a)

  def checkParam(param: ValueAttribute): OK[Unit] = dTensorTypeUtil.checkParam(
    param
  )

  def elemOK(elem: TypeAttribute): Boolean = dTensorTypeUtil.elemOK(elem)

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
          case _: dTensorNatType => dTensorTypeUtil.resolveNatValue(v.getVal()).map(_ => ())
          case ValueRefType(ref) => checkLayoutParam(ValueAttribute(ref.getVal()))
      case IntegerAttr(_, _: IndexType)   => OK(())
      case IntegerAttr(_, _: IntegerType) => OK(())

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

  def asMemref(t: dMemrefType): dMemrefMemrefType =
    t match
      case dMemrefVectorType(param, elem) =>
        dMemrefMemrefType(Seq(param), elem)
      case dMemrefMatrixType(rows, cols, elem) =>
        dMemrefMemrefType(Seq(rows, cols), elem)
      case mt: dMemrefMemrefType =>
        mt

  def sameDims(lhs: Seq[ValueAttribute], rhs: Seq[ValueAttribute]): Boolean =
    dTensorTypeUtil.sameDims(lhs, rhs)

private def parseLayoutParam[$: P](using Parser): P[LayoutParam] =
  P(
    ValueAttributeP.map(v => v: LayoutParam) |
      attrOfP[IntegerAttr].map(i => i: LayoutParam) |
      decimalLiteralP.map(v => IntegerAttr(IntData(v), IndexType()): LayoutParam)
  )

final case class dMemrefVectorType(param: DimParam, elem: TypeAttribute)
    extends dMemrefType:
  override def name: String = "d_memref.vector"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(param, elem)

  override def customVerify(): OK[Unit] =
    dMemrefTypeUtil.checkParam(param).flatMap(_ =>
      if dMemrefTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_memref.vector element type `${dMemrefTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[dMemrefVectorType]:
  override def name: String = "d_memref.vector"

  override def parse[$: P](using Parser): P[dMemrefVectorType] =
    P("<" ~ ValueAttributeP ~ "," ~ typeP ~ ">").map((param, elem) =>
      dMemrefVectorType(param, elem.asInstanceOf[TypeAttribute])
    )

final case class dMemrefMatrixType(
    rows: DimParam,
    cols: DimParam,
    elem: TypeAttribute,
) extends dMemrefType:
  override def name: String = "d_memref.matrix"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(rows, cols, elem)

  override def customVerify(): OK[Unit] =
    dMemrefTypeUtil.checkParam(rows).flatMap(_ =>
      dMemrefTypeUtil.checkParam(cols)
    ).flatMap(_ =>
      if dMemrefTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_memref.matrix element type `${dMemrefTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[dMemrefMatrixType]:
  override def name: String = "d_memref.matrix"

  override def parse[$: P](using Parser): P[dMemrefMatrixType] =
    P("<" ~ ValueAttributeP ~ "," ~ ValueAttributeP ~ "," ~ typeP ~ ">")
      .map((rows, cols, elem) =>
        dMemrefMatrixType(rows, cols, elem.asInstanceOf[TypeAttribute])
      )

final case class dMemrefMemrefType(
    params: Seq[ValueAttribute],
    elem: TypeAttribute,
    offset: Option[LayoutParam] = None,
    strides: Option[Seq[LayoutParam]] = None,
) extends dMemrefType:
  override def name: String = "d_memref.memref"

  override def parameters: Seq[Attribute | Seq[Attribute]] =
    Seq(params, elem) ++
      offset.map(dMemrefTypeUtil.layoutParamAttribute) ++
      strides.toSeq.map(_.map(dMemrefTypeUtil.layoutParamAttribute))

  override def printParameters(p: Printer): Unit =
    given indentLevel: Int = 0
    p.print("<[")
    p.printListF(params, p.print, sep = ", ")
    p.print("], ", elem)
    (offset, strides) match
      case (Some(off), Some(ss)) =>
        p.print(", offset: ")
        dMemrefTypeUtil.printLayoutParam(p, off)
        p.print(", strides: [")
        p.printListF(ss, s => dMemrefTypeUtil.printLayoutParam(p, s), sep = ", ")
        p.print("]")
      case _ => ()
    p.print(">")

  override def customVerify(): OK[Unit] =
    params.foldLeft[OK[Unit]](OK(()))((acc, p) =>
      acc.flatMap(_ => dMemrefTypeUtil.checkParam(p))
    ).flatMap(_ =>
      if dMemrefTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_memref element type `${dMemrefTypeUtil.renderAttr(elem)}`"
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
            dMemrefTypeUtil.checkLayoutParam(off).flatMap(_ =>
              ss.foldLeft[OK[Unit]](OK(()))((acc, s) =>
                acc.flatMap(_ => dMemrefTypeUtil.checkLayoutParam(s))
              )
            )
        case _ =>
          Err("d_memref.memref: offset and strides must be specified together")
    )

given AttributeCompanion[dMemrefMemrefType]:
  override def name: String = "d_memref.memref"

  override def parse[$: P](using Parser): P[dMemrefMemrefType] =
    P(
      "<" ~ "[" ~ ValueAttributeP.rep(sep = ",") ~ "]" ~ "," ~ typeP ~
        ("," ~ "offset:" ~ parseLayoutParam ~ "," ~ "strides:" ~ "[" ~
          parseLayoutParam.rep(sep = ",") ~ "]").? ~ ">"
    ).map((params, elem, layoutOpt) =>
      val (offset, strides) = layoutOpt match
        case Some((off, ss)) => (Some(off), Some(ss))
        case None            => (None, None)
      dMemrefMemrefType(
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
    res: Result[dMemrefMemrefType]
) extends DerivedOperation["d_memref.alloc", Alloc]
    derives DerivedOperationCompanion:

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " : () -> ", res.typ)

given OperationCustomParser[Alloc]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Alloc] =
    P(":" ~ "(" ~ ")" ~ "->" ~ typeOfP[dMemrefMemrefType]).flatMap(typ =>
      resultP(resNames.head, typ).map(Alloc(_))
    )

final case class Dealloc(
    memref: Operand[dMemrefMemrefType]
) extends DerivedOperation["d_memref.dealloc", Dealloc]
    derives DerivedOperationCompanion:

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", memref, " : ", memref.typ)

given OperationCustomParser[Dealloc]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Dealloc] =
    P(operandNameP ~ ":" ~ typeOfP[dMemrefMemrefType]).flatMap((mName, mTyp) =>
      operandP(mName, mTyp).map(Dealloc(_))
    )

final case class Dim(
    memref: Operand[dMemrefMemrefType],
    axis: Operand[IndexType],
    res: Result[IndexType],
) extends DerivedOperation["d_memref.dim", Dim]
    with NoMemoryEffect derives DerivedOperationCompanion:

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

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
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
      operandNameP ~ "," ~ operandNameP ~ ":" ~ typeOfP[dMemrefMemrefType] ~
        "->" ~ typeOfP[IndexType]
    ).flatMap((mName, axisName, mTyp, rTyp) =>
      operandP(mName, mTyp).flatMap(m =>
        operandP(axisName, IndexType()).flatMap(axis =>
          resultP(resNames.head, rTyp).map(r => Dim(m, axis, r))
        )
      )
    )

final case class DimExact(
    memref: Operand[dMemrefMemrefType],
    axis: IntegerAttr,
    res: Result[ValueRefType],
) extends DerivedOperation["d_memref.dim_exact", DimExact]
    with NoMemoryEffect derives DerivedOperationCompanion:

  private def selectedDimValue: OK[Value[Attribute]] =
    val idx = axis.value.value
    val rank = BigInt(memref.typ.params.size)
    if idx < 0 || idx >= rank then
      Err(s"d_memref.dim_exact: axis $idx out of bounds for rank ${memref.typ.params.size}")
    else OK(memref.typ.params(idx.toInt).getVal())

  override def customVerify(): OK[Operation] =
    if axis.typ != I32 then
      Err(s"d_memref.dim_exact: expected i32 axis attribute, got ${axis.typ}")
    else
      selectedDimValue.flatMap(sel =>
        if res.typ.ref.getVal() eq sel then
          dTensorTypeUtil.resolveNatValue(res.typ.ref.getVal()).map(_ => this)
        else
          Err(
            "d_memref.dim_exact: expected result !value<...> to reference the selected embedded dim"
          )
      )

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
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
        typeOfP[dMemrefMemrefType] ~ "->" ~ typeOfP[ValueRefType]
    ).flatMap((mName, axis, mTyp, rTyp) =>
      operandP(mName, mTyp).flatMap(m => resultP(resNames.head, rTyp).map(r =>
        DimExact(m, axis, r)
      ))
    )

final case class Load(
    memref: Operand[dMemrefMemrefType],
    indices: Seq[Operand[IndexType]],
    res: Result[TypeAttribute],
) extends DerivedOperation["d_memref.load", Load]
    derives DerivedOperationCompanion:

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

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", memref, "[")
    printer.printList(indices)
    printer.print("] : ", memref.typ, " -> ", res.typ)

given OperationCustomParser[Load]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Load] =
    P(
      operandNameP ~ "[" ~ operandNameP.rep(sep = ",") ~ "]" ~ ":" ~
        typeOfP[dMemrefMemrefType] ~ "->" ~ typeP
    ).flatMap((mName, idxNames, mTyp, rTyp) =>
      operandP(mName, mTyp).flatMap(m =>
        parseIndexOperands(idxNames).flatMap(idxs =>
          resultP(resNames.head, rTyp.asInstanceOf[TypeAttribute]).map(r =>
            Load(m, idxs, r)
          )
        )
      )
    )

final case class LinearizedLoad(
    memref: Operand[dMemrefMemrefType],
    linearIndex: Operand[IndexType],
    res: Result[TypeAttribute],
) extends DerivedOperation["d_memref.linearized_load", LinearizedLoad]
    derives DerivedOperationCompanion:

  override def customVerify(): OK[Operation] =
    if memref.typ.params.isEmpty then
      Err("d_memref.linearized_load: expected ranked memref")
    else if res.typ != memref.typ.elem then
      Err(
        s"d_memref.linearized_load: expected result type ${memref.typ.elem}, got ${res.typ}"
      )
    else OK(this)

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", memref, "[", linearIndex, "] : ", memref.typ, " -> ", res.typ)

given OperationCustomParser[LinearizedLoad]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[LinearizedLoad] =
    P(
      operandNameP ~ "[" ~ operandNameP ~ "]" ~ ":" ~
        typeOfP[dMemrefMemrefType] ~ "->" ~ typeP
    ).flatMap((mName, idxName, mTyp, rTyp) =>
      operandP(mName, mTyp).flatMap(m =>
        operandP(idxName, IndexType()).flatMap(idx =>
          resultP(resNames.head, rTyp.asInstanceOf[TypeAttribute]).map(r =>
            LinearizedLoad(m, idx, r)
          )
        )
      )
    )

final case class BasePtr(
    memref: Operand[dMemrefMemrefType],
    res: Result[llvm.Ptr],
) extends DerivedOperation["d_memref.base_ptr", BasePtr]
    with NoMemoryEffect derives DerivedOperationCompanion:

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", memref, " : ", memref.typ, " -> ", res.typ)

given OperationCustomParser[BasePtr]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[BasePtr] =
    P(
      operandNameP ~ ":" ~ typeOfP[dMemrefMemrefType] ~ "->" ~ typeOfP[llvm.Ptr]
    ).flatMap((mName, mTyp, rTyp) =>
      operandP(mName, mTyp).flatMap(m =>
        resultP(resNames.head, rTyp).map(r => BasePtr(m, r))
      )
    )

final case class LinearizedLoadFromBase(
    base: Operand[llvm.Ptr],
    linearIndex: Operand[IndexType],
    res: Result[TypeAttribute],
) extends DerivedOperation["d_memref.linearized_load_from_base", LinearizedLoadFromBase]
    derives DerivedOperationCompanion:

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", base, "[", linearIndex, "] : ", base.typ, " -> ", res.typ)

given OperationCustomParser[LinearizedLoadFromBase]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[LinearizedLoadFromBase] =
    P(
      operandNameP ~ "[" ~ operandNameP ~ "]" ~ ":" ~
        typeOfP[llvm.Ptr] ~ "->" ~ typeP
    ).flatMap((bName, idxName, bTyp, rTyp) =>
      operandP(bName, bTyp).flatMap(base =>
        operandP(idxName, IndexType()).flatMap(idx =>
          resultP(resNames.head, rTyp.asInstanceOf[TypeAttribute]).map(r =>
            LinearizedLoadFromBase(base, idx, r)
          )
        )
      )
    )

final case class ExtractStridedMetadata(
    source: Operand[dMemrefMemrefType],
    _results: Seq[Result[Attribute]],
) extends DerivedOperation["d_memref.extract_strided_metadata", ExtractStridedMetadata]
    derives DerivedOperationCompanion

object ExtractStridedMetadata:
  def baseTypeOf(srcType: dMemrefMemrefType): dMemrefMemrefType =
    dMemrefMemrefType(Seq.empty, srcType.elem)

  def resultTypesOf(srcType: dMemrefMemrefType): Seq[Attribute] =
    Seq(baseTypeOf(srcType), IndexType()) ++ Seq.fill(srcType.params.size * 2)(IndexType())

  def build(source: Operand[dMemrefMemrefType]): ExtractStridedMetadata =
    ExtractStridedMetadata(source, resultTypesOf(source.typ).map(Result(_)))

final case class Store(
    value: Operand[TypeAttribute],
    memref: Operand[dMemrefMemrefType],
    indices: Seq[Operand[IndexType]],
) extends DerivedOperation["d_memref.store", Store]
    derives DerivedOperationCompanion:

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

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", value, ", ", memref, "[")
    printer.printList(indices)
    printer.print("] : ", value.typ, ", ", memref.typ)

given OperationCustomParser[Store]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Store] =
    P(
      operandNameP ~ "," ~ operandNameP ~ "[" ~ operandNameP.rep(sep = ",") ~
        "]" ~ ":" ~ typeP ~ "," ~ typeOfP[dMemrefMemrefType]
    ).flatMap((vName, mName, idxNames, vTyp, mTyp) =>
      operandP(vName, vTyp.asInstanceOf[TypeAttribute]).flatMap(v =>
        operandP(mName, mTyp).flatMap(m =>
          parseIndexOperands(idxNames).map(idxs => Store(v, m, idxs))
        )
      )
    )

final case class Cast(
    src: Operand[dMemrefMemrefType],
    res: Result[dMemrefMemrefType],
) extends DerivedOperation["d_memref.cast", Cast]
    with NoMemoryEffect derives DerivedOperationCompanion:

  override def customVerify(): OK[Operation] =
    if src.typ.elem != res.typ.elem then
      Err(
        s"d_memref.cast: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else if src.typ.params.size != res.typ.params.size then
      Err(
        s"d_memref.cast: expected equal ranks, got ${src.typ.params.size} and ${res.typ.params.size}"
      )
    else if !dMemrefTypeUtil.sameDims(src.typ.params, res.typ.params) then
      Err("d_memref.cast: expected pairwise SSA-identical dims")
    else if !dMemrefTypeUtil.sameLayout(
        src.typ.offset,
        src.typ.strides,
        res.typ.offset,
        res.typ.strides,
      )
    then Err("d_memref.cast: expected identical layout metadata")
    else OK(this)

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", src, " : ", src.typ, " -> ", res.typ)

given OperationCustomParser[Cast]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Cast] =
    P(operandNameP ~ ":" ~ typeOfP[dMemrefMemrefType] ~ "->" ~ typeOfP[
      dMemrefMemrefType
    ]).flatMap((srcName, srcTyp, resTyp) =>
      operandP(srcName, srcTyp).flatMap(src =>
        resultP(resNames.head, resTyp).map(res => Cast(src, res))
      )
    )

final case class Subview(
    src: Operand[dMemrefMemrefType],
    offsets: Seq[Operand[IndexType]],
    sizes: Seq[Operand[IndexType]],
    strides: Seq[Operand[IndexType]],
    res: Result[dMemrefMemrefType],
) extends DerivedOperation["d_memref.subview", Subview]
    derives DerivedOperationCompanion:

  private def sameDimAsSizeOperand(
      dim: ValueAttribute,
      size: Operand[IndexType],
  ): Boolean =
    val dimNat = dTensorTypeUtil.resolveNatValue(dim.getVal())
    val sizeNat = dTensorTypeUtil.resolveNatFromIndexValue(size)
    (dimNat, sizeNat) match
      case (OK(d), OK(s)) => d eq s
      case _              =>
        (dim.getVal().owner, size.owner) match
          case (
                Some(NatConst(IntegerAttr(IntData(d), _), _)),
                Some(arith.Constant(IntegerAttr(IntData(s), _: IndexType), _)),
              ) => d == s
          case _ => false

  private def firstSizeProvenanceMismatch: Option[Int] =
    res.typ.params.zip(sizes).zipWithIndex.collectFirst {
      case ((d, s), axis) if !sameDimAsSizeOperand(d, s) => axis
    }

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
      firstSizeProvenanceMismatch match
        case Some(axis) =>
          Err(
            s"d_memref.subview: size provenance mismatch at axis $axis; expected result dim to match size operand via dtensor.shape.to_index"
          )
        case None =>
          OK(this)

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
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
        "]" ~ ":" ~ typeOfP[dMemrefMemrefType] ~ "->" ~ typeOfP[dMemrefMemrefType]
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
    src: Operand[dMemrefMemrefType],
    offset: Operand[IndexType],
    sizes: Seq[Operand[IndexType]],
    strides: Seq[Operand[IndexType]],
    res: Result[dMemrefMemrefType],
) extends DerivedOperation["d_memref.reinterpret_cast", ReinterpretCast]
    derives DerivedOperationCompanion:

  private def sameDimAsSizeOperand(
      dim: ValueAttribute,
      size: Operand[IndexType],
  ): Boolean =
    val dimNat = dTensorTypeUtil.resolveNatValue(dim.getVal())
    val sizeNat = dTensorTypeUtil.resolveNatFromIndexValue(size)
    (dimNat, sizeNat) match
      case (OK(d), OK(s)) => d eq s
      case _              =>
        (dim.getVal().owner, size.owner) match
          case (
                Some(NatConst(IntegerAttr(IntData(d), _), _)),
                Some(arith.Constant(IntegerAttr(IntData(s), _: IndexType), _)),
              ) => d == s
          case _ => false

  private def firstSizeProvenanceMismatch: Option[Int] =
    res.typ.params.zip(sizes).zipWithIndex.collectFirst {
      case ((d, s), axis) if !sameDimAsSizeOperand(d, s) => axis
    }

  override def customVerify(): OK[Operation] =
    val resRank = res.typ.params.size
    if sizes.size != resRank then
      Err(s"d_memref.reinterpret_cast: expected $resRank sizes, got ${sizes.size}")
    else if strides.size != resRank then
      Err(s"d_memref.reinterpret_cast: expected $resRank strides, got ${strides.size}")
    else if src.typ.elem != res.typ.elem then
      Err(
        s"d_memref.reinterpret_cast: expected equal element types, got ${src.typ.elem} and ${res.typ.elem}"
      )
    else
      firstSizeProvenanceMismatch match
        case Some(axis) =>
          Err(
            s"d_memref.reinterpret_cast: size provenance mismatch at axis $axis; expected result dim to match size operand via dtensor.shape.to_index"
          )
        case None => OK(this)

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", src, " to\n")
    printer.print(printer.indent * (indentLevel + 1), "offset: [", offset, "],\n")
    printer.print(printer.indent * (indentLevel + 1), "sizes: [")
    printer.printList(sizes)
    printer.print("],\n")
    printer.print(printer.indent * (indentLevel + 1), "strides: [")
    printer.printList(strides)
    printer.print("]\n")
    printer.print(printer.indent * indentLevel, ": ", src.typ, " to ", res.typ)

given OperationCustomParser[ReinterpretCast]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[ReinterpretCast] =
    P(
      operandNameP ~ "to" ~ "offset:" ~ "[" ~ operandNameP ~ "]" ~ "," ~
        "sizes:" ~ "[" ~ operandNameP.rep(sep = ",") ~ "]" ~ "," ~
        "strides:" ~ "[" ~ operandNameP.rep(sep = ",") ~ "]" ~ ":" ~
        typeOfP[dMemrefMemrefType] ~ "to" ~ typeOfP[dMemrefMemrefType]
    ).flatMap((srcName, offName, sizeNames, strideNames, srcTyp, resTyp) =>
      operandP(srcName, srcTyp).flatMap(src =>
        operandP(offName, IndexType()).flatMap(offset =>
          parseIndexOperands(sizeNames).flatMap(sizes =>
            parseIndexOperands(strideNames).flatMap(strides =>
              resultP(resNames.head, resTyp).map(res =>
                ReinterpretCast(src, offset, sizes, strides, res)
              )
            )
          )
        )
      )
    )

val dMemrefDialect = summonDialect[
  (dMemrefVectorType, dMemrefMatrixType, dMemrefMemrefType),
  (
      Alloc,
      Dealloc,
      Dim,
      DimExact,
      Load,
      LinearizedLoad,
      BasePtr,
      LinearizedLoadFromBase,
      ExtractStridedMetadata,
      Store,
      Cast,
      Subview,
      ReinterpretCast,
  ),
]

package scair.dialects.d_memref

import fastparse.*
import scair.Printer
import scair.clair.macros.*
import scair.dialects.arith
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.ir.*
import scair.parse.*
import scair.parse.given
import scair.utils.*

type DimParam = ValueAttribute

sealed trait dMemrefType extends ParametrizedAttribute, TypeAttribute

object dMemrefTypeUtil:

  def renderAttr(a: Attribute): String = dTensorTypeUtil.renderAttr(a)

  def checkParam(param: ValueAttribute): OK[Unit] = dTensorTypeUtil.checkParam(
    param
  )

  def elemOK(elem: TypeAttribute): Boolean = dTensorTypeUtil.elemOK(elem)

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
) extends dMemrefType:
  override def name: String = "d_memref.memref"

  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(params, elem)

  override def printParameters(p: Printer): Unit =
    p.print("<[")
    p.printListF(params, p.print, sep = ", ")
    p.print("], ", elem, ">")(using indentLevel = 0)

  override def customVerify(): OK[Unit] =
    params.foldLeft[OK[Unit]](OK(()))((acc, p) =>
      acc.flatMap(_ => dMemrefTypeUtil.checkParam(p))
    ).flatMap(_ =>
      if dMemrefTypeUtil.elemOK(elem) then OK(())
      else
        Err(
          s"invalid d_memref element type `${dMemrefTypeUtil.renderAttr(elem)}`"
        )
    )

given AttributeCompanion[dMemrefMemrefType]:
  override def name: String = "d_memref.memref"

  override def parse[$: P](using Parser): P[dMemrefMemrefType] =
    P("<" ~ "[" ~ ValueAttributeP.rep(sep = ",") ~ "]" ~ "," ~ typeP ~ ">")
      .map((params, elem) =>
        dMemrefMemrefType(params, elem.asInstanceOf[TypeAttribute])
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

  private def isUnitStride(v: Value[Attribute]): Boolean =
    v.owner match
      case Some(
            arith.Constant(
              IntegerAttr(IntData(1), _),
              _,
            )
          ) =>
        true
      case _ => false

  private def sameDimAsSizeOperand(
      dim: ValueAttribute,
      size: Operand[IndexType],
  ): Boolean =
    val dimNat = dTensorTypeUtil.resolveNatValue(dim.getVal())
    val sizeNat = dTensorTypeUtil.resolveNatFromIndexValue(size)
    (dimNat, sizeNat) match
      case (OK(d), OK(s)) => d eq s
      case _              => false

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
    else if !strides.forall(isUnitStride) then
      Err("d_memref.subview: only unit strides are supported in this version")
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

val dMemrefDialect = summonDialect[
  (dMemrefVectorType, dMemrefMatrixType, dMemrefMemrefType),
  (Alloc, Dealloc, Dim, DimExact, Load, Store, Cast, Subview),
]

package scair.dialects.memref

import fastparse.*
import scair.Printer
import scair.clair.codegen.*
import scair.clair.macros.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.parse.*
import scair.parse.given

private def parseIndexOperands[$: P](names: Seq[String])(using
    p: Parser
): P[Seq[Operand[IndexType]]] =
  names.foldLeft(Pass(Seq.empty[Operand[IndexType]]))((acc, n) =>
    (acc ~ operandP(n, IndexType())).map(_ :+ _)
  )

//
// ███╗░░░███╗ ███████╗ ███╗░░░███╗ ██████╗░ ███████╗ ███████╗
// ████╗░████║ ██╔════╝ ████╗░████║ ██╔══██╗ ██╔════╝ ██╔════╝
// ██╔████╔██║ █████╗░░ ██╔████╔██║ ██████╔╝ █████╗░░ █████╗░░
// ██║╚██╔╝██║ ██╔══╝░░ ██║╚██╔╝██║ ██╔══██╗ ██╔══╝░░ ██╔══╝░░
// ██║░╚═╝░██║ ███████╗ ██║░╚═╝░██║ ██║░░██║ ███████╗ ██║░░░░░
// ╚═╝░░░░░╚═╝ ╚══════╝ ╚═╝░░░░░╚═╝ ╚═╝░░╚═╝ ╚══════╝ ╚═╝░░░░░
//

case class Alloc(
    dynamicSizes: Seq[Operand[IndexType]],
    symbolOperands: Seq[Operand[IndexType]],
    memref: Result[MemrefType],
    alignment: IntegerAttr,
) extends DerivedOperation["memref.alloc", Alloc]
    derives DerivedOperationCompanion

case class Dealloc(
    memref: Operand[MemrefType]
) extends DerivedOperation["memref.dealloc", Dealloc]
    with AssemblyFormat["$memref attr-dict `:` type($memref)"]
    derives DerivedOperationCompanion

case class Dim(
    memref: Operand[MemrefType],
    index: Operand[IndexType],
    result: Result[IndexType],
) extends DerivedOperation["memref.dim", Dim]
    with NoMemoryEffect derives DerivedOperationCompanion

case class Load(
    memref: Operand[MemrefType],
    indices: Seq[Operand[IndexType]],
    result: Result[Attribute],
) extends DerivedOperation["memref.load", Load]
    derives DerivedOperationCompanion

case class Store(
    value: Operand[Attribute],
    memref: Operand[MemrefType],
    indices: Seq[Operand[IndexType]],
) extends DerivedOperation["memref.store", Store]
    derives DerivedOperationCompanion

case class ReinterpretCast(
    src: Operand[MemrefType],
    offset: Operand[IndexType],
    sizes: Seq[Operand[IndexType]],
    strides: Seq[Operand[IndexType]],
    res: Result[MemrefType],
) extends DerivedOperation["memref.reinterpret_cast", ReinterpretCast]
    derives DerivedOperationCompanion:

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
        typeOfP[MemrefType] ~ "to" ~ typeOfP[MemrefType]
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

case class ExtractStridedMetadata(
    source: Operand[MemrefType],
    _results: Seq[Result[Attribute]],
) extends DerivedOperation["memref.extract_strided_metadata", ExtractStridedMetadata]
    derives DerivedOperationCompanion

object ExtractStridedMetadata:
  def baseTypeOf(srcType: MemrefType): RankedMemrefType =
    srcType match
      case ranked: RankedMemrefType =>
        RankedMemrefType(ranked.elementType, ArrayAttribute(Seq.empty))
      case unranked =>
        RankedMemrefType(unranked.asInstanceOf[UnrankedMemrefType].elementType, ArrayAttribute(Seq.empty))

  def resultTypesOf(srcType: MemrefType): Seq[Attribute] =
    val rank = srcType match
      case ranked: RankedMemrefType => ranked.shape.attrValues.size
      case _                        => 0
    Seq(baseTypeOf(srcType), IndexType()) ++ Seq.fill(rank * 2)(IndexType())

  def build(source: Operand[MemrefType]): ExtractStridedMetadata =
    ExtractStridedMetadata(source, resultTypesOf(source.typ).map(Result(_)))

val MemrefDialect =
  summonDialect[
    EmptyTuple,
    (
        Alloc,
        Dealloc,
        Load,
        Store,
        Dim,
        ReinterpretCast,
        ExtractStridedMetadata,
    ),
  ]

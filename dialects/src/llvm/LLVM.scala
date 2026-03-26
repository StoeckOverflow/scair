package scair.dialects.llvm

import fastparse.*
import scair.Printer
import scair.clair.macros.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.parse.*
import scair.parse.given

case class Ptr() extends DerivedAttribute["llvm.ptr", Ptr] with TypeAttribute
    derives DerivedAttributeCompanion

final case class StructType(
    elems: Seq[TypeAttribute]
) extends ParametrizedAttribute
    with TypeAttribute:
  override def name: String = "llvm.struct"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(elems)

  override def printParameters(p: Printer): Unit =
    given indentLevel: Int = 0
    p.print("<(")
    p.printListF(elems, p.print, sep = ", ")
    p.print(")>")

given AttributeCompanion[StructType]:
  override def name: String = "llvm.struct"

  override def parse[$: P](using Parser): P[StructType] =
    P("<" ~ "(" ~ typeP.rep(sep = ",") ~ ")" ~ ">").map(elems =>
      StructType(elems.map(_.asInstanceOf[TypeAttribute]))
    )

final case class ArrayType(
    size: IntData,
    elem: TypeAttribute,
) extends ParametrizedAttribute
    with TypeAttribute:
  override def name: String = "llvm.array"
  override def parameters: Seq[Attribute | Seq[Attribute]] = Seq(size, elem)

  override def printParameters(p: Printer): Unit =
    given indentLevel: Int = 0
    p.print("<", size, " x ", elem, ">")

given AttributeCompanion[ArrayType]:
  override def name: String = "llvm.array"

  override def parse[$: P](using Parser): P[ArrayType] =
    P("<" ~ decimalLiteralP ~ "x" ~ typeP ~ ">").map((size, elem) =>
      ArrayType(IntData(size), elem.asInstanceOf[TypeAttribute])
    )

case class Constant(
    value: Attribute,
    res: Result[Attribute],
) extends DerivedOperation["llvm.mlir.constant", Constant]
    with NoMemoryEffect derives DerivedOperationCompanion

case class Zero(
    res: Result[Attribute]
) extends DerivedOperation["llvm.mlir.zero", Zero]
    with NoMemoryEffect derives DerivedOperationCompanion

case class Poison(
    res: Result[Attribute]
) extends DerivedOperation["llvm.mlir.poison", Poison]
    with AssemblyFormat["attr-dict `:` type($res)"]
    with NoMemoryEffect derives DerivedOperationCompanion

case class Add(
    lhs: Operand[IntegerType | IndexType],
    rhs: Operand[IntegerType | IndexType],
    res: Result[IntegerType | IndexType],
    overflowFlags: Option[ArrayAttribute[StringData]] = None,
) extends DerivedOperation["llvm.add", Add]
    derives DerivedOperationCompanion

case class Mul(
    lhs: Operand[IntegerType | IndexType],
    rhs: Operand[IntegerType | IndexType],
    res: Result[IntegerType | IndexType],
    overflowFlags: Option[ArrayAttribute[StringData]] = None,
) extends DerivedOperation["llvm.mul", Mul]
    derives DerivedOperationCompanion

case class FAdd(
    lhs: Operand[FloatType],
    rhs: Operand[FloatType],
    res: Result[FloatType],
) extends DerivedOperation["llvm.fadd", FAdd]
    derives DerivedOperationCompanion

case class ICmp(
    lhs: Operand[IntegerType | IndexType],
    rhs: Operand[IntegerType | IndexType],
    predicate: StringData,
    res: Result[IntegerType],
) extends DerivedOperation["llvm.icmp", ICmp]
    derives DerivedOperationCompanion

case class Load(
    addr: Operand[Ptr],
    res: Result[Attribute],
) extends DerivedOperation["llvm.load", Load]
    with AssemblyFormat["$addr attr-dict `:` type($addr) `->` type($res)"]
    derives DerivedOperationCompanion

case class Store(
    value: Operand[Attribute],
    addr: Operand[Ptr],
) extends DerivedOperation["llvm.store", Store]
    derives DerivedOperationCompanion

case class GetElementPtr(
    base: Operand[Ptr],
    dynamicIndices: Seq[Operand[IntegerType | IndexType]],
    res: Result[Ptr],
    rawConstantIndices: DenseArrayAttr,
    elem_type: Attribute,
    gepFlags: Option[ArrayAttribute[StringData]] = None,
) extends DerivedOperation["llvm.getelementptr", GetElementPtr]
    with NoMemoryEffect derives DerivedOperationCompanion

case class ExtractValue(
    container: Operand[Attribute],
    position: DenseArrayAttr,
    res: Result[Attribute],
) extends DerivedOperation["llvm.extractvalue", ExtractValue]
    derives DerivedOperationCompanion

case class InsertValue(
    value: Operand[Attribute],
    container: Operand[Attribute],
    position: DenseArrayAttr,
    res: Result[Attribute],
) extends DerivedOperation["llvm.insertvalue", InsertValue]
    derives DerivedOperationCompanion

case class PtrToInt(
    in: Operand[Ptr],
    out: Result[IntegerType | IndexType],
) extends DerivedOperation["llvm.ptrtoint", PtrToInt]
    derives DerivedOperationCompanion

case class IntToPtr(
    in: Operand[IntegerType | IndexType],
    out: Result[Ptr],
) extends DerivedOperation["llvm.inttoptr", IntToPtr]
    derives DerivedOperationCompanion

case class Call(
    callee: SymbolRefAttr,
    operandss: Seq[Operand[Attribute]],
    resultss: Seq[Result[Attribute]],
) extends DerivedOperation["llvm.call", Call] derives DerivedOperationCompanion

case class Br(
    args: Seq[Operand[Attribute]],
    dest: Block,
) extends DerivedOperation["llvm.br", Br]
    with IsTerminator derives DerivedOperationCompanion

case class CondBr(
    condition: Operand[IntegerType],
    trueArgs: Seq[Operand[Attribute]],
    falseArgs: Seq[Operand[Attribute]],
    trueDest: Block,
    falseDest: Block,
) extends DerivedOperation["llvm.cond_br", CondBr]
    with IsTerminator derives DerivedOperationCompanion

case class Return(
    args: Seq[Operand[Attribute]]
) extends DerivedOperation["llvm.return", Return]
    with IsTerminator derives DerivedOperationCompanion

val LLVMDialect = summonDialect[
  (Ptr, StructType, ArrayType),
  (
      Constant,
      Zero,
      Poison,
      Add,
      Mul,
      FAdd,
      ICmp,
      Load,
      Store,
      GetElementPtr,
      ExtractValue,
      InsertValue,
      PtrToInt,
      IntToPtr,
      Call,
      Br,
      CondBr,
      Return,
  ),
]

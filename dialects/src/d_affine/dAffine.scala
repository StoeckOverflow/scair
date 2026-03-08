package scair.dialects.d_affine

import fastparse.*
import scair.Printer
import scair.clair.macros.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.parse.*
import scair.utils.*

final case class Apply(
    args: Seq[Operand[IndexType]],
    map: AffineMapAttr,
    res: Result[IndexType],
) extends DerivedOperation["d_affine.apply", Apply]
    with NoMemoryEffect derives DerivedOperationCompanion:

  private def expectedArity: Int =
    map.affineMap.dimensions.size + map.affineMap.symbols.size

  override def customVerify(): OK[Operation] =
    if args.size != expectedArity then
      Err(
        s"d_affine.apply: expected $expectedArity index operands for map ${map.affineMap}, got ${args.size}"
      )
    else if map.affineMap.affineExprs.size != 1 then
      Err(
        s"d_affine.apply: only single-result affine maps are supported, got ${map.affineMap.affineExprs.size} results"
      )
    else OK(this)

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", map, "(")
    printer.printList(args)
    printer.print(") : ")
    if args.nonEmpty then
      printer.print("(")
      printer.printList(args.map(_.typ))
      printer.print(")")
    else printer.print("()")
    printer.print(" -> ", res.typ)

given OperationCustomParser[Apply]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Apply] =
    P(
      attrOfP[AffineMapAttr] ~ "(" ~ operandNameP.rep(sep = ",") ~ ")" ~ ":" ~
        "(" ~ typeP.rep(sep = ",") ~ ")" ~ "->" ~ typeOfP[IndexType]
    ).flatMap((map, operandNames, operandTypes, resTy) =>
      if operandTypes.exists(_ != IndexType()) then
        Fail("d_affine.apply: expected all operand types to be index")
      else
        operandNames.zip(operandTypes).foldLeft(Pass(Seq.empty[Operand[IndexType]])) {
          case (acc, (name, typ)) =>
            acc.flatMap(seq =>
              operandP(name, typ.asInstanceOf[IndexType]).map(seq :+ _)
            )
        }.flatMap(ops =>
          resultP(resNames.head, resTy).map(res => Apply(ops, map, res))
        )
    )

final case class For(
    lb: Operand[IndexType],
    ub: Operand[IndexType],
    step: IntegerAttr,
    body: Region,
) extends DerivedOperation["d_affine.for", For]
    with NoTerminator derives DerivedOperationCompanion:

  private def verifyBodyShape(): OK[Unit] =
    if body.blocks.size != 1 then
      Err("d_affine.for: expected a single-block body")
    else
      val block = body.blocks.head
      if block.arguments.size != 1 then
        Err("d_affine.for: expected exactly one induction variable block argument")
      else
        block.arguments.head.typ match
          case _: IndexType => OK(())
          case other             =>
            Err(s"d_affine.for: expected induction variable type index, got $other")

  private def verifyStepPositive(): OK[Unit] =
    if step.value.value > 0 then OK(())
    else Err(s"d_affine.for: expected positive step, got ${step.value.value}")

  override def customVerify(): OK[Operation] =
    verifyBodyShape().flatMap(_ =>
      verifyStepPositive()
    ).flatMap(_ =>
      body.blocks.head.operations.lastOption match
        case Some(_: Yield) => OK(this)
        case Some(other)    =>
          Err(s"d_affine.for: expected terminator d_affine.yield, got `${other.name}`")
        case None           =>
          Err("d_affine.for: expected non-empty body terminated by d_affine.yield")
    )

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    val block = body.blocks.head
    val iv = block.arguments.head
    printer.print(name, " ", iv, " = ", lb, " to ", ub, " step ", step, " {\n")
    for op <- block.operations do printer.print(op)(using indentLevel + 1)
    printer.print(printer.indent * indentLevel, "}")

given OperationCustomParser[For]:
  def parse[$: P](resNames: Seq[String])(using p: Parser): P[For] =
    P(
      operandNameP ~ "=" ~ operandNameP ~ "to" ~ operandNameP ~ "step" ~
        attrOfP[IntegerAttr]
    ).flatMap((ivName, lbName, ubName, step) =>
      operandP(lbName, IndexType()).flatMap(lb =>
        operandP(ubName, IndexType()).flatMap(ub =>
          regionP(Seq(ivName -> IndexType())).map(body =>
            For(lb, ub, step, body)
          )
        )
      )
    )

final case class Yield()
    extends DerivedOperation["d_affine.yield", Yield]
    with IsTerminator
    with NoMemoryEffect derives DerivedOperationCompanion:

  override def customVerify(): OK[Operation] =
    containerBlock.flatMap(_.containerRegion).flatMap(_.containerOperation) match
      case Some(_: For) => OK(this)
      case Some(other)  =>
        Err(s"d_affine.yield: expected parent op d_affine.for, got `${other.name}`")
      case None         =>
        Err("d_affine.yield: expected to be nested in d_affine.for body")

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name)

given OperationCustomParser[Yield]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Yield] =
    Pass(Yield())

final case class Min(
    lhs: Operand[IndexType],
    rhs: Operand[IndexType],
    res: Result[IndexType],
) extends DerivedOperation["d_affine.min", Min]
    with NoMemoryEffect derives DerivedOperationCompanion:

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", lhs, ", ", rhs, " : (index, index) -> index")

given OperationCustomParser[Min]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Min] =
    P(operandNameP ~ "," ~ operandNameP ~ ":" ~ "(" ~ typeP ~ "," ~ typeP ~
      ")" ~ "->" ~ typeP).flatMap((lhsName, rhsName, lhsTyp, rhsTyp, resTyp) =>
      if lhsTyp != IndexType() || rhsTyp != IndexType() ||
          resTyp != IndexType()
      then
        Fail("d_affine.min: expected (index, index) -> index")
      else
        operandP(lhsName, IndexType()).flatMap(lhs =>
          operandP(rhsName, IndexType()).flatMap(rhs =>
            resultP(resNames.head, IndexType()).map(res =>
              Min(lhs, rhs, res)
            )
          )
        )
    )

val dAffineDialect = summonDialect[
  EmptyTuple,
  (Apply, For, Yield, Min),
]

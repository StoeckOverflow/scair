package scair.dialects.d_affine

import fastparse.*
import scair.Printer
import scair.clair.macros.*
import scair.dialects.builtin.*
import scair.dialects.dTensor.*
import scair.ir.*
import scair.parse.*
import scair.utils.*

final case class For(
    lb: Operand[dTensorNatType],
    ub: Operand[dTensorNatType],
    step: Operand[dTensorNatType],
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
          case _: dTensorNatType => OK(())
          case other             =>
            Err(s"d_affine.for: expected induction variable type !dtensor.nat, got $other")

  private def verifyStepPositiveIfConstant(): OK[Unit] =
    step.owner match
      case Some(NatConst(IntegerAttr(IntData(v), _), _)) =>
        if v > 0 then OK(()) else Err(s"d_affine.for: expected positive step, got $v")
      case _ => OK(())

  override def customVerify(): OK[Operation] =
    verifyBodyShape().flatMap(_ =>
      verifyStepPositiveIfConstant()
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
        operandNameP
    ).flatMap((ivName, lbName, ubName, stepName) =>
      operandP(lbName, dTensorNatType()).flatMap(lb =>
        operandP(ubName, dTensorNatType()).flatMap(ub =>
          operandP(stepName, dTensorNatType()).flatMap(step =>
            regionP(Seq(ivName -> dTensorNatType())).map(body =>
              For(lb, ub, step, body)
            )
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
    lhs: Operand[dTensorNatType],
    rhs: Operand[dTensorNatType],
    res: Result[dTensorNatType],
) extends DerivedOperation["d_affine.min", Min]
    with NoMemoryEffect derives DerivedOperationCompanion:

  override def customPrint(printer: Printer)(using indentLevel: Int): Unit =
    printer.print(name, " ", lhs, ", ", rhs, " : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat")

given OperationCustomParser[Min]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Min] =
    P(operandNameP ~ "," ~ operandNameP ~ ":" ~ "(" ~ typeP ~ "," ~ typeP ~
      ")" ~ "->" ~ typeP).flatMap((lhsName, rhsName, lhsTyp, rhsTyp, resTyp) =>
      if lhsTyp != dTensorNatType() || rhsTyp != dTensorNatType() ||
          resTyp != dTensorNatType()
      then
        Fail("d_affine.min: expected (!dtensor.nat, !dtensor.nat) -> !dtensor.nat")
      else
        operandP(lhsName, dTensorNatType()).flatMap(lhs =>
          operandP(rhsName, dTensorNatType()).flatMap(rhs =>
            resultP(resNames.head, dTensorNatType()).map(res =>
              Min(lhs, rhs, res)
            )
          )
        )
    )

val dAffineDialect = summonDialect[
  EmptyTuple,
  (For, Yield, Min),
]

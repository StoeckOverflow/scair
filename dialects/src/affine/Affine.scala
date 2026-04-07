package scair.dialects.affine

import fastparse.*
import scair.Printer
import scair.clair.*
import scair.dialects.builtin.*
import scair.ir.*
import scair.parse.*

// ░█████╗░ ███████╗ ███████╗ ██╗ ███╗░░██╗ ███████╗
// ██╔══██╗ ██╔════╝ ██╔════╝ ██║ ████╗░██║ ██╔════╝
// ███████║ █████╗░░ █████╗░░ ██║ ██╔██╗██║ █████╗░░
// ██╔══██║ ██╔══╝░░ ██╔══╝░░ ██║ ██║╚████║ ██╔══╝░░
// ██║░░██║ ██║░░░░░ ██║░░░░░ ██║ ██║░╚███║ ███████╗
// ╚═╝░░╚═╝ ╚═╝░░░░░ ╚═╝░░░░░ ╚═╝ ╚═╝░░╚══╝ ╚══════╝

// ░█████╗░ ██████╗░ ███████╗ ██████╗░ ░█████╗░ ████████╗ ██╗ ░█████╗░ ███╗░░██╗ ░██████╗
// ██╔══██╗ ██╔══██╗ ██╔════╝ ██╔══██╗ ██╔══██╗ ╚══██╔══╝ ██║ ██╔══██╗ ████╗░██║ ██╔════╝
// ██║░░██║ ██████╔╝ █████╗░░ ██████╔╝ ███████║ ░░░██║░░░ ██║ ██║░░██║ ██╔██╗██║ ╚█████╗░
// ██║░░██║ ██╔═══╝░ ██╔══╝░░ ██╔══██╗ ██╔══██║ ░░░██║░░░ ██║ ██║░░██║ ██║╚████║ ░╚═══██╗
// ╚█████╔╝ ██║░░░░░ ███████╗ ██║░░██║ ██║░░██║ ░░░██║░░░ ██║ ╚█████╔╝ ██║░╚███║ ██████╔╝
// ░╚════╝░ ╚═╝░░░░░ ╚══════╝ ╚═╝░░╚═╝ ╚═╝░░╚═╝ ░░░╚═╝░░░ ╚═╝ ░╚════╝░ ╚═╝░░╚══╝ ╚═════╝░

/*≡==---==≡≡≡≡==---=≡≡*\
||      APPLY OP      ||
\*≡==----==≡≡==----==≡*/

case class Apply(
    mapOperands: Seq[Operand[IndexType]],
    res: Result[IndexType],
    map: AffineMapAttr,
) extends DerivedOperation["affine.apply"]
    with NoMemoryEffect derives OpDefs

/*≡==---=≡≡≡≡=---=≡≡*\
||      FOR OP      ||
\*≡==----=≡≡=----==≡*/

case class For(
    lowerBoundOperands: Seq[Operand[IndexType]],
    upperBoundOperands: Seq[Operand[IndexType]],
    inits: Seq[Operand[Attribute]],
    res: Seq[Result[Attribute]],
    lowerBoundMap: AffineMapAttr,
    upperBoundMap: AffineMapAttr,
    step: IntegerAttr,
    body: Region,
) extends DerivedOperation["affine.for"] derives OpDefs:

  override def customPrint(printer: Printer): Unit =
    val block = body.blocks.head
    val iv = block.arguments.head
    printer.print(name, " ", iv, " = ", lowerBoundMap, "(")
    printer.printList(lowerBoundOperands)
    printer.print(") to ", upperBoundMap, "(")
    printer.printList(upperBoundOperands)
    printer.print(") step ", step)
    if inits.nonEmpty then
      printer.print(" iter_args(")
      val iterArgs = block.arguments.tail
      printer.printListF(iterArgs.zip(inits), pair =>
        val (iterArg, init) = pair
        printer.print(iterArg, " = ", init, " : ", init.typ)
      )
      printer.print(")")
    printer.print(" {\n")
    printer.indented(block.operations.foreach(printer.print))
    printer.withIndent(printer.print("}"))

given OperationCustomParser[For]:
  def parse[$: P](resNames: Seq[String])(using p: Parser): P[For] =
    P(
      operandNameP ~ "=" ~ attrOfP[AffineMapAttr] ~ "(" ~ operandNameP.rep(
        sep = ","
      ) ~ ")" ~ "to" ~ attrOfP[AffineMapAttr] ~ "(" ~ operandNameP.rep(
        sep = ","
      ) ~ ")" ~ "step" ~ attrOfP[IntegerAttr] ~ ("iter_args" ~ "(" ~
        (operandNameP ~ "=" ~ operandNameP ~ ":" ~ typeP).rep(
          sep = ","
        ) ~ ")").?
    ).flatMap((ivName, lbMap, lbNames, ubMap, ubNames, step, iterArgsOpt) =>
      lbNames
        .foldLeft(Pass(Seq.empty[Operand[IndexType]]))((acc, n) =>
          acc.flatMap(seq => operandP(n, IndexType()).map(seq :+ _))
        )
        .flatMap(lbOps =>
          ubNames
            .foldLeft(Pass(Seq.empty[Operand[IndexType]]))((acc, n) =>
              acc.flatMap(seq => operandP(n, IndexType()).map(seq :+ _))
            )
            .flatMap(ubOps =>
              val iterArgs = iterArgsOpt.getOrElse(Seq.empty)
              val iterArgNamesAndTys =
                iterArgs.map((iterName, _, ty) => (iterName, ty))
              if resNames.size != iterArgs.size then
                Fail(
                  s"affine.for: expected ${iterArgs.size} result names to match iter_args arity, got ${resNames.size}"
                )
              else
                iterArgs.foldLeft(Pass(Seq.empty[Operand[Attribute]])) {
                  case (acc, (_, initName, ty)) =>
                    acc.flatMap(seq =>
                      operandP(initName, ty.asInstanceOf[TypeAttribute]).map(seq :+ _)
                    )
                }.flatMap(inits =>
                  resNames
                    .zip(iterArgs.map(_._3))
                    .foldLeft(Pass(Seq.empty[Result[Attribute]])) {
                      case (acc, (resName, ty)) =>
                        acc.flatMap(seq =>
                          resultP(resName, ty.asInstanceOf[TypeAttribute]).map(seq :+ _)
                        )
                    }.flatMap(results =>
                      regionP(Seq(ivName -> IndexType()) ++ iterArgNamesAndTys).map(body =>
                        For(
                          lbOps,
                          ubOps,
                          inits,
                          results,
                          lbMap,
                          ubMap,
                          step,
                          body,
                        )
                      )
                    )
                )
            )
        )
    )

/*≡==---==≡≡≡≡≡==---=≡≡*\
||     PARALLEL OP     ||
\*≡==----==≡≡≡==----==≡*/

case class Parallel(
    mapOperands: Seq[Operand[IndexType]],
    steps: Option[ArrayAttribute[IntegerAttr]],
    reductions: Attribute,
    lowerBoundsMap: AffineMapAttr,
    lowerBoundsGroups: DenseIntOrFPElementsAttr,
    upperBoundsMap: AffineMapAttr,
    upperBoundsGroups: DenseIntOrFPElementsAttr,
    res: Seq[Result[Attribute]],
    body: Region,
) extends DerivedOperation["affine.parallel"] derives OpDefs

/*≡==--=≡≡≡=--=≡≡*\
||     IF OP     ||
\*≡==---=≡=---==≡*/

case class If(
    args: Seq[Operand[Attribute]],
    res: Seq[Result[Attribute]],
    condition: AffineSetAttr,
    thenRegion: Region,
    elseRegion: Region,
) extends DerivedOperation["affine.if"] derives OpDefs

/*≡==--=≡≡≡≡=--=≡≡*\
||    STORE OP    ||
\*≡==--==≡≡==--==≡*/

case class Store(
    value: Operand[Attribute],
    memref: Operand[MemrefType],
    indices: Seq[Operand[IndexType]],
    map: AffineMapAttr,
) extends DerivedOperation["affine.store"] derives OpDefs

/*≡==---=≡≡≡=---=≡≡*\
||     LOAD OP     ||
\*≡==----=≡=----==≡*/

case class Load(
    memref: Operand[MemrefType],
    indices: Seq[Operand[IndexType]],
    result: Result[Attribute],
    map: AffineMapAttr,
) extends DerivedOperation["affine.load"] derives OpDefs

/*≡==--=≡≡≡≡=--=≡≡*\
||     MIN OP     ||
\*≡==---=≡≡=---==≡*/

case class Min(
    arguments: Seq[Operand[IndexType]],
    result: Result[IndexType],
    map: AffineMapAttr,
) extends DerivedOperation["affine.min"]
    with NoMemoryEffect derives OpDefs

/*≡==--=≡≡≡≡=--=≡≡*\
||    YIELD OP    ||
\*≡==---=≡≡=---==≡*/

case class Yield(
    arguments: Seq[Operand[Attribute]]
) extends DerivedOperation["affine.yield"]
    with IsTerminator
    with AssemblyFormat["attr-dict ($arguments^ `:` type($arguments))?"]
    with NoMemoryEffect derives OpDefs

val AffineDialect = summonDialect[
  EmptyTuple,
  (Apply, For, Parallel, If, Store, Load, Min, Yield),
]

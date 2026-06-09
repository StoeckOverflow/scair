package scair.dialects.d_affine

import fastparse.*
import scair.print.Printer
import scair.clair.*
import scair.dialects.builtin.*
import scair.dialects.d_tensor
import scair.dialects.d_memref
import scair.ir.*
import scair.parse.*
import scair.parse.given
import scair.utils.*

private def isAffineIndexLikeType(a: Attribute): Boolean =
  a match
    case _: IndexType => true
    case _: d_tensor.DTensorSizeWitnessType => true
    case _ => false

private def parseAffineIndexLikeOperand[$: P](
    name: String,
    annotatedType: Attribute,
    owner: String,
)(using Parser): P[Operand[Attribute]] =
  if !isAffineIndexLikeType(annotatedType) then
    Fail(s"$owner: expected operand type index or d_tensor size witness, got $annotatedType")
  else
    existingOrForwardValueRefOperandP(name).flatMap { valueAttr =>
      val v = valueAttr.getVal()
      if isAffineIndexLikeType(v.typ) then Pass(v.asInstanceOf[Operand[Attribute]])
      else Fail(s"$owner: expected operand %$name to have type index or d_tensor size witness, got ${v.typ}")
    }

final case class Apply(
    dimOperands: Seq[Operand[Attribute]],
    symbolOperands: Seq[Operand[Attribute]],
    map: AffineMapAttr,
    res: Result[IndexType],
) extends DerivedOperation["d_affine.apply"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if res.typ != IndexType() then
      Err(s"d_affine.apply: expected result type index, got ${res.typ}")
    else if (dimOperands ++ symbolOperands).exists(op => !isAffineIndexLikeType(op.typ)) then
      Err("d_affine.apply: expected all dim/symbol operands to be index or d_tensor size witnesses")
    else if dimOperands.size != map.affineMap.dimensions.size then
      Err(
        s"d_affine.apply: expected ${map.affineMap.dimensions.size} dim operands for map ${map.affineMap}, got ${dimOperands.size}"
      )
    else if symbolOperands.size != map.affineMap.symbols.size then
      Err(
        s"d_affine.apply: expected ${map.affineMap.symbols.size} symbol operands for map ${map.affineMap}, got ${symbolOperands.size}"
      )
    else if map.affineMap.affineExprs.size != 1 then
      Err(
        s"d_affine.apply: only single-result affine maps are supported, got ${map.affineMap.affineExprs.size} results"
      )
    else OK(this)

  override def customPrint(printer: Printer): Unit =
    printer.print(name, " ", map, " (")
    printer.printList(dimOperands)
    printer.print(")[")
    printer.printList(symbolOperands)
    printer.print("] : (")
    printer.printList(dimOperands.map(_.typ))
    printer.print(")[")
    printer.printList(symbolOperands.map(_.typ))
    printer.print("]")
    printer.print(" -> ", res.typ)

given OperationCustomParser[Apply]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Apply] =
    P(
      attrOfP[AffineMapAttr] ~ "(" ~ operandNameP.rep(sep = ",") ~ ")" ~ "[" ~
        operandNameP.rep(sep = ",") ~ "]" ~ ":" ~ "(" ~ typeP.rep(sep = ",") ~
        ")" ~ "[" ~ typeP.rep(sep = ",") ~ "]" ~ "->" ~ typeOfP[IndexType]
    ).flatMap((map, dimNames, symNames, dimTypes, symTypes, resTy) =>
      if dimNames.size != dimTypes.size then
        Fail(
          s"d_affine.apply: expected equal dim operand name/type arity, got ${dimNames.size} names and ${dimTypes.size} types"
        )
      else if symNames.size != symTypes.size then
        Fail(
          s"d_affine.apply: expected equal symbol operand name/type arity, got ${symNames.size} names and ${symTypes.size} types"
        )
      else if dimTypes.exists(t => t != IndexType() && !t.isInstanceOf[d_tensor.DTensorSizeWitnessType]) ||
        symTypes.exists(t => t != IndexType() && !t.isInstanceOf[d_tensor.DTensorSizeWitnessType])
      then Fail("d_affine.apply: expected all dim/symbol operand types to be index or d_tensor size witnesses")
      else
        dimNames
          .zip(dimTypes)
          .foldLeft(Pass(Seq.empty[Operand[Attribute]])) {
            case (acc, (name, typ)) =>
              acc.flatMap(seq =>
                parseAffineIndexLikeOperand(name, typ, "d_affine.apply").map(seq :+ _)
              )
          }
          .flatMap(dimOps =>
            symNames
              .zip(symTypes)
              .foldLeft(Pass(Seq.empty[Operand[Attribute]])) {
                case (acc, (name, typ)) =>
                  acc.flatMap(seq =>
                    parseAffineIndexLikeOperand(name, typ, "d_affine.apply").map(seq :+ _)
                  )
              }
              .flatMap(symOps =>
                resultP(resNames.head, resTy).map(res =>
                  Apply(dimOps, symOps, map, res)
                )
              )
          )
    )

final case class For(
    lowerBoundOperands: Seq[Operand[Attribute]],
    upperBoundOperands: Seq[Operand[Attribute]],
    stepOperands: Seq[Operand[Attribute]],
    inits: Seq[Operand[Attribute]],
    res: Seq[Result[Attribute]],
    lowerBoundMap: AffineMapAttr,
    upperBoundMap: AffineMapAttr,
    step: IntegerAttr,
    body: Region,
) extends DerivedOperation["d_affine.for"]
    with NoTerminator derives OpDefs:

  private def isAffineIndexLike(a: Attribute): Boolean =
    a match
      case _: IndexType => true
      case _: d_tensor.DTensorSizeWitnessType => true
      case _ => false

  private def expectedArity(map: AffineMapAttr): Int =
    map.affineMap.dimensions.size + map.affineMap.symbols.size

  private def verifyBoundContract(): OK[Unit] =
    if lowerBoundMap.affineMap.affineExprs.size != 1 then
      Err(
        s"d_affine.for: only single-result lower bound maps are supported, got ${lowerBoundMap.affineMap.affineExprs.size} results"
      )
    else if upperBoundMap.affineMap.affineExprs.size != 1 then
      Err(
        s"d_affine.for: only single-result upper bound maps are supported, got ${upperBoundMap.affineMap.affineExprs.size} results"
      )
    else if lowerBoundOperands.size != expectedArity(lowerBoundMap) then
      Err(
        s"d_affine.for: expected ${expectedArity(lowerBoundMap)} lower bound operands for map ${lowerBoundMap.affineMap}, got ${lowerBoundOperands.size}"
      )
    else if upperBoundOperands.size != expectedArity(upperBoundMap) then
      Err(
        s"d_affine.for: expected ${expectedArity(upperBoundMap)} upper bound operands for map ${upperBoundMap.affineMap}, got ${upperBoundOperands.size}"
      )
    else if (lowerBoundOperands ++ upperBoundOperands ++ stepOperands).exists(op => !isAffineIndexLike(op.typ)) then
      Err("d_affine.for: expected lower/upper/step operands to be index or d_tensor size witnesses")
    else OK(())

  private def verifyInitResultContract(): OK[Unit] =
    if inits.size != res.size then
      Err(
        s"d_affine.for: expected equal init/result arity, got ${inits.size} and ${res.size}"
      )
    else if inits.zip(res).exists((init, r) => init.typ != r.typ) then
      val bad = inits.zip(res).zipWithIndex.collectFirst {
        case ((init, r), idx) if init.typ != r.typ => (idx, init.typ, r.typ)
      }.get
      Err(
        s"d_affine.for: init/result type mismatch at position ${bad._1}; expected ${bad._2}, got ${bad._3}"
      )
    else OK(())

  private def verifyStepContract(): OK[Unit] =
    if stepOperands.size > 1 then
      Err(s"d_affine.for: expected at most one dynamic step operand, got ${stepOperands.size}")
    else if stepOperands.nonEmpty then OK(())
    else if step.value.value > 0 then OK(())
    else Err(s"d_affine.for: expected positive step, got ${step.value.value}")

  private def verifyBodyShape(): OK[Unit] =
    if body.blocks.size != 1 then
      Err("d_affine.for: expected a single-block body")
    else
      val block = body.blocks.head
      val expectedArgs = 1 + inits.size
      if block.arguments.size != expectedArgs then
        Err(
          s"d_affine.for: expected $expectedArgs block arguments (iv + ${inits.size} iter args), got ${block.arguments.size}"
        )
      else
        block.arguments.head.typ match
          case _: IndexType =>
            val iterArgs = block.arguments.tail
            val mismatch = iterArgs.zip(inits).zipWithIndex.collectFirst {
              case ((iterArg, init), idx) if iterArg.typ != init.typ =>
                (idx, init.typ, iterArg.typ)
            }
            mismatch match
              case Some((idx, expected, got)) =>
                Err(
                  s"d_affine.for: iter arg type mismatch at position $idx; expected $expected, got $got"
                )
              case None => OK(())
          case other =>
            Err(s"d_affine.for: expected induction variable type index, got $other")

  private def verifyTerminatorContract(): OK[Unit] =
    body.blocks.head.operations.lastOption match
      case Some(y: Yield) =>
        if y.args.size != res.size then
          Err(
            s"d_affine.for: expected d_affine.yield to have ${res.size} operands, got ${y.args.size}"
          )
        else
          val mismatch = y.args.zip(res).zipWithIndex.collectFirst {
            case ((arg, r), idx) if arg.typ != r.typ =>
              (idx, r.typ, arg.typ)
          }
          mismatch match
            case Some((idx, expected, got)) =>
              Err(
                s"d_affine.for: yield/result type mismatch at position $idx; expected $expected, got $got"
              )
            case None =>
              OK(())
      case Some(other) =>
        Err(s"d_affine.for: expected terminator d_affine.yield, got `${other.name}`")
      case None =>
        Err("d_affine.for: expected non-empty body terminated by d_affine.yield")

  override def customVerify(): OK[Operation] =
    verifyBoundContract().flatMap(_ =>
      verifyInitResultContract()
    ).flatMap(_ =>
      verifyBodyShape()
    ).flatMap(_ =>
      verifyStepContract()
    ).flatMap(_ =>
      verifyTerminatorContract().map(_ => this)
    )

  override def customPrint(printer: Printer): Unit =
    val block = body.blocks.head
    val iv = block.arguments.head
    printer.print(name, " ", iv, " = ", lowerBoundMap, "(")
    printer.printList(lowerBoundOperands)
    printer.print(") to ", upperBoundMap, "(")
    printer.printList(upperBoundOperands)
    printer.print(") step ")
    stepOperands.headOption match
      case Some(dynamicStep) => printer.print(dynamicStep, " : ", dynamicStep.typ)
      case None              => printer.print(step)
    if inits.nonEmpty then
      printer.print(" iter_args(")
      val iterArgs = block.arguments.tail
      printer.printListF(iterArgs.zip(inits), pair =>
        val (iterArg, init) = pair
        printer.print(iterArg, " = ", init, " : ", init.typ)
      )
      printer.print(")")
    printer.print(" {\n")
    printer.printBlockBody(block)
    printer.withIndent(printer.print("}"))

private enum ForStepSpec:
  case Static(step: IntegerAttr)
  case Dynamic(name: String, typ: TypeAttribute)

private def forStepSpecP[$: P](using Parser): P[ForStepSpec] =
  P(
    operandNameP.flatMap(name =>
      P(":" ~ typeP).flatMap {
        case typ: TypeAttribute if isAffineIndexLikeType(typ) =>
          Pass(ForStepSpec.Dynamic(name, typ))
        case other =>
          Fail(s"d_affine.for: expected dynamic step type index or d_tensor size witness, got $other")
      }
    ) | attrOfP[IntegerAttr].map(ForStepSpec.Static(_))
  )

given OperationCustomParser[For]:
  def parse[$: P](resNames: Seq[String])(using p: Parser): P[For] =
    P(
      operandNameP ~ "=" ~ attrOfP[AffineMapAttr] ~ "(" ~ operandNameP.rep(
        sep = ","
      ) ~ ")" ~ "to" ~ attrOfP[AffineMapAttr] ~ "(" ~ operandNameP.rep(
        sep = ","
      ) ~ ")" ~ "step" ~ forStepSpecP ~ ("iter_args" ~ "(" ~
        (operandNameP ~ "=" ~ operandNameP ~ ":" ~ typeP).rep(
          sep = ","
        ) ~ ")").?
    ).flatMap((ivName, lbMap, lbNames, ubMap, ubNames, stepSpec, iterArgsOpt) =>
      lbNames
        .foldLeft(Pass(Seq.empty[Operand[Attribute]]))((acc, n) =>
          acc.flatMap(seq =>
            existingOrForwardValueRefOperandP(n).flatMap { valueAttr =>
              val v = valueAttr.getVal()
              if isAffineIndexLikeType(v.typ) then Pass(seq :+ v.asInstanceOf[Operand[Attribute]])
              else Fail(s"d_affine.for: expected lower bound operand %$n to have type index or d_tensor size witness, got ${v.typ}")
            }
          )
        )
        .flatMap(lbOps =>
          ubNames
            .foldLeft(Pass(Seq.empty[Operand[Attribute]]))((acc, n) =>
              acc.flatMap(seq =>
                existingOrForwardValueRefOperandP(n).flatMap { valueAttr =>
                  val v = valueAttr.getVal()
                  if isAffineIndexLikeType(v.typ) then Pass(seq :+ v.asInstanceOf[Operand[Attribute]])
                  else Fail(s"d_affine.for: expected upper bound operand %$n to have type index or d_tensor size witness, got ${v.typ}")
                }
              )
            )
            .flatMap(ubOps =>
              val iterArgs = iterArgsOpt.getOrElse(Seq.empty)
              val iterArgNamesAndTys =
                iterArgs.map((iterName, _, ty) => (iterName, ty))
              if resNames.size != iterArgs.size then
                Fail(
                  s"d_affine.for: expected ${iterArgs.size} result names to match iter_args arity, got ${resNames.size}"
                )
              else
                val parsedStep =
                  stepSpec match
                    case ForStepSpec.Static(step) =>
                      Pass((Seq.empty[Operand[Attribute]], step))
                    case ForStepSpec.Dynamic(name, typ) =>
                      parseAffineIndexLikeOperand(name, typ, "d_affine.for").map(stepOperand =>
                        (Seq(stepOperand), IntegerAttr(IntData(1), I32))
                      )
                parsedStep.flatMap { case (stepOperands, step) =>
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
                            stepOperands,
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
                }
            )
        )
    )

final case class Yield(
    args: Seq[Operand[Attribute]]
)
    extends DerivedOperation["d_affine.yield"]
    with IsTerminator
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    containerBlock.flatMap(_.containerRegion).flatMap(_.containerOperation) match
      case Some(f: For) =>
        if args.size != f.res.size then
          Err(
            s"d_affine.yield: expected ${f.res.size} operands to match parent results, got ${args.size}"
          )
        else
          val mismatch = args.zip(f.res).zipWithIndex.collectFirst {
            case ((arg, r), idx) if arg.typ != r.typ => (idx, r.typ, arg.typ)
          }
          mismatch match
            case Some((idx, expected, got)) =>
              Err(
                s"d_affine.yield: operand type mismatch at position $idx; expected $expected, got $got"
              )
            case None =>
              OK(this)
      case Some(ifOp: If) =>
        if args.size != ifOp.res.size then
          Err(
            s"d_affine.yield: expected ${ifOp.res.size} operands to match parent results, got ${args.size}"
          )
        else
          val mismatch = args.zip(ifOp.res).zipWithIndex.collectFirst {
            case ((arg, r), idx) if arg.typ != r.typ => (idx, r.typ, arg.typ)
          }
          mismatch match
            case Some((idx, expected, got)) =>
              Err(
                s"d_affine.yield: operand type mismatch at position $idx; expected $expected, got $got"
              )
            case None =>
              OK(this)
      case Some(other)  =>
        Err(s"d_affine.yield: expected parent op d_affine.for or d_affine.if, got `${other.name}`")
      case None         =>
        Err("d_affine.yield: expected to be nested in d_affine.for or d_affine.if body")

  override def customPrint(printer: Printer): Unit =
    if args.isEmpty then
      printer.print(name)
    else
      printer.print(name, " ")
      printer.printList(args)
      printer.print(" : (")
      printer.printList(args.map(_.typ))
      printer.print(")")

given OperationCustomParser[Yield]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Yield] =
    P(
      (operandNameP.rep(sep = ",") ~ ":" ~ "(" ~ typeP.rep(sep = ",") ~ ")")
        .?
    ).flatMap {
      case None => Pass(Yield(Seq.empty))
      case Some((argNames, argTypes)) =>
        if argNames.size != argTypes.size then
          Fail(
            s"d_affine.yield: expected equal operand name/type arity, got ${argNames.size} names and ${argTypes.size} types"
          )
        else
          argNames.zip(argTypes).foldLeft(Pass(Seq.empty[Operand[Attribute]])) {
            case (acc, (name, typ)) =>
              acc.flatMap(seq =>
                operandP(name, typ.asInstanceOf[TypeAttribute]).map(seq :+ _)
              )
          }.map(args => Yield(args))
    }

final case class Min(
    dimOperands: Seq[Operand[Attribute]],
    symbolOperands: Seq[Operand[Attribute]],
    map: AffineMapAttr,
    res: Result[IndexType],
) extends DerivedOperation["d_affine.min"]
    with NoMemoryEffect derives OpDefs:

  override def customVerify(): OK[Operation] =
    if res.typ != IndexType() then
      Err(s"d_affine.min: expected result type index, got ${res.typ}")
    else if (dimOperands ++ symbolOperands).exists(op => !isAffineIndexLikeType(op.typ)) then
      Err("d_affine.min: expected all dim/symbol operands to be index or d_tensor size witnesses")
    else if dimOperands.size != map.affineMap.dimensions.size then
      Err(
        s"d_affine.min: expected ${map.affineMap.dimensions.size} dim operands for map ${map.affineMap}, got ${dimOperands.size}"
      )
    else if symbolOperands.size != map.affineMap.symbols.size then
      Err(
        s"d_affine.min: expected ${map.affineMap.symbols.size} symbol operands for map ${map.affineMap}, got ${symbolOperands.size}"
      )
    else if map.affineMap.affineExprs.isEmpty then
      Err(
        "d_affine.min: expected at least one affine expression"
      )
    else OK(this)

  override def customPrint(printer: Printer): Unit =
    printer.print(name, " ", map, " (")
    printer.printList(dimOperands)
    printer.print(")[")
    printer.printList(symbolOperands)
    printer.print("] : (")
    printer.printList(dimOperands.map(_.typ))
    printer.print(")[")
    printer.printList(symbolOperands.map(_.typ))
    printer.print("] -> ", res.typ)

given OperationCustomParser[Min]:
  def parse[$: P](resNames: Seq[String])(using Parser): P[Min] =
    P(
      attrOfP[AffineMapAttr] ~ "(" ~ operandNameP.rep(sep = ",") ~ ")" ~ "[" ~
        operandNameP.rep(sep = ",") ~ "]" ~ ":" ~ "(" ~ typeP.rep(sep = ",") ~
        ")" ~ "[" ~ typeP.rep(sep = ",") ~ "]" ~ "->" ~ typeOfP[IndexType]
    ).flatMap((map, dimNames, symNames, dimTypes, symTypes, resTy) =>
      if dimNames.size != dimTypes.size then
        Fail(
          s"d_affine.min: expected equal dim operand name/type arity, got ${dimNames.size} names and ${dimTypes.size} types"
        )
      else if symNames.size != symTypes.size then
        Fail(
          s"d_affine.min: expected equal symbol operand name/type arity, got ${symNames.size} names and ${symTypes.size} types"
        )
      else if dimTypes.exists(t => t != IndexType() && !t.isInstanceOf[d_tensor.DTensorSizeWitnessType]) ||
        symTypes.exists(t => t != IndexType() && !t.isInstanceOf[d_tensor.DTensorSizeWitnessType])
      then Fail("d_affine.min: expected all dim/symbol operand types to be index or d_tensor size witnesses")
      else
        dimNames
          .zip(dimTypes)
          .foldLeft(Pass(Seq.empty[Operand[Attribute]])) {
            case (acc, (name, typ)) =>
              acc.flatMap(seq =>
                parseAffineIndexLikeOperand(name, typ, "d_affine.min").map(seq :+ _)
              )
          }
          .flatMap(dimOps =>
            symNames
              .zip(symTypes)
              .foldLeft(Pass(Seq.empty[Operand[Attribute]])) {
                case (acc, (name, typ)) =>
                  acc.flatMap(seq =>
                    parseAffineIndexLikeOperand(name, typ, "d_affine.min").map(seq :+ _)
                  )
              }
              .flatMap(symOps =>
                resultP(resNames.head, resTy).map(res =>
                  Min(dimOps, symOps, map, res)
                )
              )
          )
    )

final case class Load(
    memref: Operand[d_memref.DMemrefMemrefType],
    mapOperands: Seq[Operand[IndexType]],
    map: AffineMapAttr,
    result: Result[TypeAttribute],
) extends DerivedOperation["d_affine.load"]
    derives OpDefs:

  private def expectedMapArity: Int =
    map.affineMap.dimensions.size + map.affineMap.symbols.size

  override def customVerify(): OK[Operation] =
    if result.typ != memref.typ.elem then
      Err(
        s"d_affine.load: expected result type ${memref.typ.elem}, got ${result.typ}"
      )
    else if mapOperands.size != expectedMapArity then
      Err(
        s"d_affine.load: expected $expectedMapArity map operands for map ${map.affineMap}, got ${mapOperands.size}"
      )
    else if map.affineMap.affineExprs.size != memref.typ.params.size then
      Err(
        s"d_affine.load: expected ${memref.typ.params.size} map results for memref rank ${memref.typ.params.size}, got ${map.affineMap.affineExprs.size}"
      )
    else OK(this)

final case class Store(
    value: Operand[TypeAttribute],
    memref: Operand[d_memref.DMemrefMemrefType],
    mapOperands: Seq[Operand[IndexType]],
    map: AffineMapAttr,
) extends DerivedOperation["d_affine.store"]
    derives OpDefs:

  private def expectedMapArity: Int =
    map.affineMap.dimensions.size + map.affineMap.symbols.size

  override def customVerify(): OK[Operation] =
    if value.typ != memref.typ.elem then
      Err(
        s"d_affine.store: expected stored value type ${memref.typ.elem}, got ${value.typ}"
      )
    else if mapOperands.size != expectedMapArity then
      Err(
        s"d_affine.store: expected $expectedMapArity map operands for map ${map.affineMap}, got ${mapOperands.size}"
      )
    else if map.affineMap.affineExprs.size != memref.typ.params.size then
      Err(
        s"d_affine.store: expected ${memref.typ.params.size} map results for memref rank ${memref.typ.params.size}, got ${map.affineMap.affineExprs.size}"
      )
    else OK(this)

final case class If(
    args: Seq[Operand[IndexType]],
    condition: AffineSetAttr,
    thenRegion: Region,
    elseRegion: Region,
    res: Seq[Result[Attribute]],
) extends DerivedOperation["d_affine.if"]
    with NoTerminator derives OpDefs:

  private def verifyRegion(name: String, region: Region): OK[Unit] =
    if region.blocks.size != 1 then
      Err(s"d_affine.if: expected $name region to contain a single block")
    else
      val block = region.blocks.head
      block.operations.lastOption match
        case Some(y: Yield) =>
          if y.args.size != res.size then
            Err(
              s"d_affine.if: expected $name d_affine.yield to have ${res.size} operands, got ${y.args.size}"
            )
          else
            val mismatch = y.args.zip(res).zipWithIndex.collectFirst {
              case ((arg, r), idx) if arg.typ != r.typ =>
                (idx, r.typ, arg.typ)
            }
            mismatch match
              case Some((idx, expected, got)) =>
                Err(
                  s"d_affine.if: $name yield/result type mismatch at position $idx; expected $expected, got $got"
                )
              case None =>
                OK(())
        case Some(other) if res.nonEmpty =>
          Err(s"d_affine.if: expected $name region terminator d_affine.yield, got `${other.name}`")
        case None if res.nonEmpty =>
          Err(s"d_affine.if: expected non-empty $name region terminated by d_affine.yield")
        case _ =>
          OK(())

  override def customVerify(): OK[Operation] =
    verifyRegion("then", thenRegion).flatMap(_ =>
      verifyRegion("else", elseRegion).map(_ => this)
    )

final case class Parallel(
    mapOperands: Seq[Operand[IndexType]],
    steps: Option[ArrayAttribute[IntegerAttr]],
    reductions: Attribute,
    lowerBoundsMap: AffineMapAttr,
    lowerBoundsGroups: DenseIntOrFPElementsAttr,
    upperBoundsMap: AffineMapAttr,
    upperBoundsGroups: DenseIntOrFPElementsAttr,
    res: Seq[Result[Attribute]],
    body: Region,
) extends DerivedOperation["d_affine.parallel"]
    with NoTerminator derives OpDefs

val DAffineDialect = summonDialect[
  EmptyTuple,
  (Apply, For, Yield, Min, Load, Store, If, Parallel),
]

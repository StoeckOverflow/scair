package scair.interpreter

import scair.dialects.builtin.FunctionType
import scair.dialects.builtin.SymbolRefAttr
import scair.dialects.func
import scair.ir.Attribute
import scair.ir.Result

final case class RuntimeFunctionHandle(
    symbol: String,
    functionType: FunctionType,
)

private def runFunctionWithArgs(
    callee: func.Func,
    args: Seq[Any],
    argTypes: Seq[Attribute],
    resultTypes: Seq[Attribute],
    callSite: String,
    interpreter: Interpreter,
    ctx: RuntimeCtx,
): Option[Any] =
  val expectedIn = callee.function_type.inputs
  val expectedOut = callee.function_type.outputs

  if args.length != expectedIn.length then
    throw new Exception(
      s"$callSite: arity mismatch, expected ${expectedIn
          .length} args but got ${args.length}"
    )
  if argTypes != expectedIn then
    throw new Exception(
      s"$callSite: type mismatch, argument types $argTypes do not match callee input types $expectedIn"
    )
  if resultTypes != expectedOut then
    throw new Exception(
      s"$callSite: type mismatch, result types $resultTypes do not match callee output types $expectedOut"
    )

  val entry = callee.body.blocks.headOption.getOrElse(
    throw new Exception(
      s"$callSite: callee @${callee.sym_name.stringLiteral} has no entry block"
    )
  )

  if entry.arguments.length != expectedIn.length then
    throw new Exception(
      s"$callSite: callee @${callee.sym_name.stringLiteral} entry block has ${entry
          .arguments.length} args but function type expects ${expectedIn.length}"
    )

  val new_ctx = ctx.push_scope(callee.sym_name.stringLiteral)
  interpreter.scopes += new_ctx.scopedDict

  for (operand, param) <- args.zip(entry.arguments) do
    new_ctx.scopedDict.update(param, operand)

  for op <- entry.operations do interpreter.interpret_op(op, new_ctx)

  if expectedOut.nonEmpty then
    if new_ctx.result.isEmpty then
      throw new Exception(
        s"$callSite: callee @${callee.sym_name.stringLiteral} did not produce a return value"
      )
    Some(new_ctx.result.get)
  else None

// assume one return value for now
object run_return extends OpImpl[func.Return]:

  def compute(
      op: func.Return,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    args match
      case Seq(v) => ctx.result = Some(v)
      case _      => ctx.result = Some(args)
    None

object run_call extends OpImpl[func.Call]:

  def compute(
      op: func.Call,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    // if call for print, print
    // later there may be a print operation instead
    if op.callee.rootRef.stringLiteral == "print" then
      val print_value = interpreter.lookup_op(op._operands.head, ctx)
      interpreter.interpreter_print(print_value)
      None
    else
      val calleeName = op.callee.rootRef.stringLiteral
      val callee = interpreter.symbolTable.get(calleeName)
        .getOrElse(
          throw new Exception(s"func.call: missing symbol @$calleeName")
        ) match
        case f: func.Func => f
        case other        =>
          throw new Exception(
            s"func.call: symbol @$calleeName does not resolve to func.func (got ${other
                .name})"
          )

      runFunctionWithArgs(
        callee = callee,
        args = args,
        argTypes = op._operands.map(_.typ),
        resultTypes = op._results.map(_.typ),
        callSite = "func.call",
        interpreter = interpreter,
        ctx = ctx,
      )

object run_func_constant extends OpImpl[func.Constant]:

  def compute(
      op: func.Constant,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    Some(
      RuntimeFunctionHandle(
        symbol = op.value.rootRef.stringLiteral,
        functionType = op.res.typ,
      )
    )

object run_call_indirect extends OpImpl[func.CallIndirect]:

  def compute(
      op: func.CallIndirect,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =
    val handleVal = args.headOption
      .getOrElse(
        throw new Exception("func.call_indirect: non-handle callee <missing>")
      )
    val handle = handleVal match
      case h: RuntimeFunctionHandle => h
      case other                    =>
        throw new Exception(
          s"func.call_indirect: non-handle callee $other"
        )

    if handle.functionType != op.callee.typ then
      throw new Exception(
        s"func.call_indirect: type mismatch, handle type ${handle
            .functionType} does not match callee operand type ${op.callee.typ}"
      )

    val callee = interpreter.symbolTable.get(handle.symbol)
      .getOrElse(
        throw new Exception(
          s"func.call_indirect: missing symbol @${handle.symbol}"
        )
      ) match
      case f: func.Func => f
      case other        =>
        throw new Exception(
          s"func.call_indirect: symbol @${handle
              .symbol} does not resolve to func.func (got ${other.name})"
        )

    if callee.function_type != handle.functionType then
      throw new Exception(
        s"func.call_indirect: type mismatch, handle type ${handle.functionType} does not match target function type ${callee
            .function_type}"
      )

    runFunctionWithArgs(
      callee = callee,
      args = args.drop(1),
      argTypes = op.callee_operands.map(_.typ),
      resultTypes = op._results.map(_.typ),
      callSite = "func.call_indirect",
      interpreter = interpreter,
      ctx = ctx,
    )

object run_function extends OpImpl[func.Func]:

  // only needed for main
  def compute(
      op: func.Func,
      interpreter: Interpreter,
      ctx: RuntimeCtx,
      args: Seq[Any],
  ): Option[Any] =

    // if main function, call it immediately
    if op.sym_name.stringLiteral == "main" then
      val new_call = func.Call(
        callee = SymbolRefAttr(op.sym_name),
        _operands = Seq(),
        _results = op.function_type.outputs.map(res => Result(res)),
      )
      // should it be external call like xDSL?
      run_call.run(new_call, interpreter, ctx)
      // get return value from main call and add to context
      val return_result = interpreter.lookup_op(new_call._results.head, ctx)
      ctx.result = Some(return_result)
    None

val InterpreterFuncDialect: InterpreterDialect =
  Seq(
    run_func_constant,
    run_call_indirect,
    run_return,
    run_call,
    run_function,
  )

package scair.interpreter

import scair.ir.*

type InterpreterDialect = Seq[OpImpl[? <: Operation]]

val allInterpreterDialects: Seq[InterpreterDialect] =
  Seq(
    InterpreterBuiltinDialect,
    InterpreterFuncDialect,
    InterpreterArithDialect,
    InterpreterdTensorDialect,
    InterpreterMemrefDialect,
    InterpreterdMemrefDialect,
    InterpreterdAffineDialect,
  )

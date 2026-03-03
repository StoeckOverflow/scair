// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam | filecheck %s --check-prefix=BETA -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize | filecheck %s --check-prefix=MONO -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam | filecheck %s --check-prefix=ERASE -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam,lower-tlam-to-func | filecheck %s --check-prefix=LOWER -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam,canonicalize,cse,monomorphize,erase-tlam,lower-tlam-to-func,canonicalize,cse | filecheck %s --check-prefix=FULL -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam,canonicalize,cse,monomorphize,erase-tlam,lower-tlam-to-func,canonicalize,cse,beta-reduce-tlam,canonicalize,cse,monomorphize,erase-tlam,lower-tlam-to-func,canonicalize,cse | filecheck %s --check-prefix=FULL2 -DFILE=%s

// VALID: polymorphic program that monomorphizes, erases TLam, then lowers.
builtin.module {
  %outer = "tlam.tlambda"() ({
    %poly_id = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    %spec = "tlam.tapply"(%poly_id) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%spec) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam.tlambda"() ({
// VERIFY:     %1 = "tlam.tlambda"() ({
// VERIFY:       %2 = "tlam.vlambda"() ({
// VERIFY:       ^bb0(%3: !tlam.bvar<0>):
// VERIFY:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// VERIFY:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// VERIFY:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// VERIFY:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// VERIFY:     %2 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// VERIFY:     "tlam.treturn"(%2) : (!tlam.fun<i64, i64>) -> ()
// VERIFY:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam.tlambda"() ({
// BETA:     %1 = "tlam.tlambda"() ({
// BETA:       %2 = "tlam.vlambda"() ({
// BETA:       ^bb0(%3: !tlam.bvar<0>):
// BETA:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// BETA:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// BETA:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// BETA:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// BETA:     %2 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// BETA:     "tlam.treturn"(%2) : (!tlam.fun<i64, i64>) -> ()
// BETA:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:     %1 = "tlam.vlambda"() ({
// MONO:     ^bb0(%2: i64):
// MONO:       "tlam.vreturn"(%2) : (i64) -> ()
// MONO:     }) : () -> !tlam.fun<i64, i64>
// MONO:     "tlam.treturn"(%1) : (!tlam.fun<i64, i64>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// MONO: }
// ERASE: builtin.module {
// ERASE:   %0 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%1: i64):
// ERASE:     "tlam.vreturn"(%1) : (i64) -> ()
// ERASE:   }) : () -> !tlam.fun<i64, i64>
// ERASE: }
// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i64) -> i64 {
// LOWER:     func.return %0 : i64
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i64) -> i64
// LOWER: }
// FULL: builtin.module {
// FULL:   func.func @lifted_1(%0: i64) -> i64 {
// FULL:     func.return %0 : i64
// FULL:   }
// FULL:   %0 = func.constant @lifted_1 : (i64) -> i64
// FULL: }
// FULL2: builtin.module {
// FULL2:   func.func @lifted_1(%0: i64) -> i64 {
// FULL2:     func.return %0 : i64
// FULL2:   }
// FULL2:   %0 = func.constant @lifted_1 : (i64) -> i64
// FULL2: }

// -----

// VALID: vapply lowering yields func.call_indirect.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %x = "test.op"() : () -> (i32)
  %r = "tlam.vapply"(%id, %x) : (!tlam.fun<i32, i32>, i32) -> (i32)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam.vlambda"() ({
// VERIFY:   ^bb0(%1: i32):
// VERIFY:     "tlam.vreturn"(%1) : (i32) -> ()
// VERIFY:   }) : () -> !tlam.fun<i32, i32>
// VERIFY:   %1 = "test.op"() : () -> i32
// VERIFY:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam.vlambda"() ({
// BETA:   ^bb0(%1: i32):
// BETA:     "tlam.vreturn"(%1) : (i32) -> ()
// BETA:   }) : () -> !tlam.fun<i32, i32>
// BETA:   %1 = "test.op"() : () -> i32
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam.vlambda"() ({
// MONO:   ^bb0(%1: i32):
// MONO:     "tlam.vreturn"(%1) : (i32) -> ()
// MONO:   }) : () -> !tlam.fun<i32, i32>
// MONO:   %1 = "test.op"() : () -> i32
// MONO:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// MONO: }
// ERASE: builtin.module {
// ERASE:   %0 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%1: i32):
// ERASE:     "tlam.vreturn"(%1) : (i32) -> ()
// ERASE:   }) : () -> !tlam.fun<i32, i32>
// ERASE:   %1 = "test.op"() : () -> i32
// ERASE:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// ERASE: }
// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i32) -> i32 {
// LOWER:     func.return %0 : i32
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i32) -> i32
// LOWER:   %1 = "test.op"() : () -> i32
// LOWER:   %2 = "func.call_indirect"(%0, %1) : ((i32) -> i32, i32) -> i32
// LOWER: }
// FULL: builtin.module {
// FULL:   func.func @lifted_1(%0: i32) -> i32 {
// FULL:     func.return %0 : i32
// FULL:   }
// FULL:   %0 = "test.op"() : () -> i32
// FULL: }
// FULL2: builtin.module {
// FULL2:   func.func @lifted_1(%0: i32) -> i32 {
// FULL2:     func.return %0 : i32
// FULL2:   }
// FULL2:   %0 = "test.op"() : () -> i32
// FULL2: }

// -----

// VALID: beta-reduce removes direct vapply(vlambda, arg).
builtin.module {
  "test.pipeline.beta"() : () -> ()
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %x = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %r = "tlam.vapply"(%id, %x) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%r) : (i32) -> ()
}
// VERIFY: builtin.module {
// VERIFY:   "test.pipeline.beta"() : () -> ()
// VERIFY:   %0 = "tlam.vlambda"() ({
// VERIFY:   ^bb0(%1: i32):
// VERIFY:     "tlam.vreturn"(%1) : (i32) -> ()
// VERIFY:   }) : () -> !tlam.fun<i32, i32>
// VERIFY:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// VERIFY:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// VERIFY:   "test.use"(%2) : (i32) -> ()
// VERIFY: }
// BETA: builtin.module {
// BETA:   "test.pipeline.beta"() : () -> ()
// BETA:   %0 = "tlam.vlambda"() ({
// BETA:   ^bb0(%1: i32):
// BETA:     "tlam.vreturn"(%1) : (i32) -> ()
// BETA:   }) : () -> !tlam.fun<i32, i32>
// BETA:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// BETA:   "test.use"(%1) : (i32) -> ()
// BETA: }
// MONO: builtin.module {
// MONO:   "test.pipeline.beta"() : () -> ()
// MONO:   %0 = "tlam.vlambda"() ({
// MONO:   ^bb0(%1: i32):
// MONO:     "tlam.vreturn"(%1) : (i32) -> ()
// MONO:   }) : () -> !tlam.fun<i32, i32>
// MONO:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// MONO:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// MONO:   "test.use"(%2) : (i32) -> ()
// MONO: }
// ERASE: builtin.module {
// ERASE:   "test.pipeline.beta"() : () -> ()
// ERASE:   %0 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%1: i32):
// ERASE:     "tlam.vreturn"(%1) : (i32) -> ()
// ERASE:   }) : () -> !tlam.fun<i32, i32>
// ERASE:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// ERASE:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// ERASE:   "test.use"(%2) : (i32) -> ()
// ERASE: }
// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i32) -> i32 {
// LOWER:     func.return %0 : i32
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i32) -> i32
// LOWER:   "test.pipeline.beta"() : () -> ()
// LOWER:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// LOWER:   %2 = "func.call_indirect"(%0, %1) : ((i32) -> i32, i32) -> i32
// LOWER:   "test.use"(%2) : (i32) -> ()
// LOWER: }
// FULL: builtin.module {
// FULL:   func.func @lifted_1(%0: i32) -> i32 {
// FULL:     func.return %0 : i32
// FULL:   }
// FULL:   "test.pipeline.beta"() : () -> ()
// FULL:   %0 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// FULL:   "test.use"(%0) : (i32) -> ()
// FULL: }
// FULL2: builtin.module {
// FULL2:   func.func @lifted_1(%0: i32) -> i32 {
// FULL2:     func.return %0 : i32
// FULL2:   }
// FULL2:   "test.pipeline.beta"() : () -> ()
// FULL2:   %0 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// FULL2:   "test.use"(%0) : (i32) -> ()
// FULL2: }

// -----

// INVALID: DBI out-of-bounds bvar in type.
builtin.module {
  %0 = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<1>):
      "tlam.vreturn"(%x) : (!tlam.bvar<1>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<1>>>)
}
// VERIFY: debruijn: bvar<1> out of scope at depth=1
// BETA: debruijn: bvar<1> out of scope at depth=1
// MONO: debruijn: bvar<1> out of scope at depth=1
// ERASE: debruijn: bvar<1> out of scope at depth=1
// LOWER: debruijn: bvar<1> out of scope at depth=1
// FULL: debruijn: bvar<1> out of scope at depth=1
// FULL2: debruijn: bvar<1> out of scope at depth=1

// -----

// INVALID: TLambda has one block arg (must be zero).
builtin.module {
  %0 = "tlam.tlambda"() ({
  ^bb0(%a: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb0(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    "tlam.treturn"(%id) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i32, i32>>)
}
// VERIFY: tlambda: must have exactly one block with zero args
// BETA: tlambda: must have exactly one block with zero args
// MONO: tlambda: must have exactly one block with zero args
// ERASE: tlambda: must have exactly one block with zero args
// LOWER: tlambda: must have exactly one block with zero args
// FULL: tlambda: must have exactly one block with zero args
// FULL2: tlambda: must have exactly one block with zero args

// -----

// INVALID: missing VReturn terminator in VLambda.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "test.op"() : () -> ()
  }) : () -> (!tlam.fun<i32, i32>)
}
// VERIFY: vlambda: last op must be tlam.vreturn, got 'test.op'
// BETA: vlambda: last op must be tlam.vreturn, got 'test.op'
// MONO: vlambda: last op must be tlam.vreturn, got 'test.op'
// ERASE: vlambda: last op must be tlam.vreturn, got 'test.op'
// LOWER: vlambda: last op must be tlam.vreturn, got 'test.op'
// FULL: vlambda: last op must be tlam.vreturn, got 'test.op'
// FULL2: vlambda: last op must be tlam.vreturn, got 'test.op'

// -----

// INVALID: tapply operand is not forall.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %bad = "tlam.tapply"(%id) <{tyArg = i32}> : (!tlam.fun<i32, i32>) -> (i32)
}
// VERIFY: tapply: expected operand of type tlam.forall, got !tlam.fun<i32, i32>
// BETA: tapply: expected operand of type tlam.forall, got !tlam.fun<i32, i32>
// MONO: tapply: expected operand of type tlam.forall, got !tlam.fun<i32, i32>
// ERASE: tapply: expected operand of type tlam.forall, got !tlam.fun<i32, i32>
// LOWER: tapply: expected operand of type tlam.forall, got !tlam.fun<i32, i32>
// FULL: tapply: expected operand of type tlam.forall, got !tlam.fun<i32, i32>
// FULL2: tapply: expected operand of type tlam.forall, got !tlam.fun<i32, i32>

// -----

// INVALID: tapply annotated result type is not instantiate(forall, arg).
builtin.module {
  %poly = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
  %bad = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i32>)
}
// VERIFY: tapply: result !tlam.fun<i64, i32> != instantiated !tlam.fun<i64, i64>
// BETA: tapply: result !tlam.fun<i64, i32> != instantiated !tlam.fun<i64, i64>
// MONO: tapply: result !tlam.fun<i64, i32> != instantiated !tlam.fun<i64, i64>
// ERASE: tapply: result !tlam.fun<i64, i32> != instantiated !tlam.fun<i64, i64>
// LOWER: tapply: result !tlam.fun<i64, i32> != instantiated !tlam.fun<i64, i64>
// FULL: tapply: result !tlam.fun<i64, i32> != instantiated !tlam.fun<i64, i64>
// FULL2: tapply: result !tlam.fun<i64, i32> != instantiated !tlam.fun<i64, i64>

// -----

// INVALID: tapply type argument is not a TypeAttribute.
builtin.module {
  %poly = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
  %bad = "tlam.tapply"(%poly) <{tyArg = "oops"}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i32, i32>)
}
// VERIFY: tapply: expected type argument, got "oops"
// BETA: tapply: expected type argument, got "oops"
// MONO: tapply: expected type argument, got "oops"
// ERASE: tapply: expected type argument, got "oops"
// LOWER: tapply: expected type argument, got "oops"
// FULL: tapply: expected type argument, got "oops"
// FULL2: tapply: expected type argument, got "oops"

// -----

// VALID regression: capture-avoiding specialization through nested TLambda.
builtin.module {
  %0 = "tlam.tlambda"() ({
    %g = "tlam.tlambda"() ({
      %h = "tlam.tlambda"() ({
        %u = "test.op"() : () -> (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>)
        "tlam.treturn"(%u) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
      }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>)
      "tlam.treturn"(%h) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
    }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>)
    %spec = "tlam.tapply"(%g) <{tyArg = !tlam.bvar<0>}> : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>) -> (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>)
    "tlam.treturn"(%spec) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
  }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam.tlambda"() ({
// VERIFY:     %1 = "tlam.tlambda"() ({
// VERIFY:       %2 = "tlam.tlambda"() ({
// VERIFY:         %3 = "test.op"() : () -> !tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>
// VERIFY:         "tlam.treturn"(%3) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
// VERIFY:       }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// VERIFY:       "tlam.treturn"(%2) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
// VERIFY:     }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>
// VERIFY:     %2 = "tlam.tapply"(%1) <{tyArg = !tlam.bvar<0>}> : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>) -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// VERIFY:     "tlam.treturn"(%2) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
// VERIFY:   }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam.tlambda"() ({
// BETA:     %1 = "tlam.tlambda"() ({
// BETA:       %2 = "tlam.tlambda"() ({
// BETA:         %3 = "test.op"() : () -> !tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>
// BETA:         "tlam.treturn"(%3) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
// BETA:       }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// BETA:       "tlam.treturn"(%2) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
// BETA:     }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>
// BETA:     %2 = "tlam.tapply"(%1) <{tyArg = !tlam.bvar<0>}> : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>) -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// BETA:     "tlam.treturn"(%2) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
// BETA:   }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:     %1 = "tlam.tlambda"() ({
// MONO:       %2 = "test.op"() : () -> !tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>
// MONO:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
// MONO:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// MONO:     "tlam.treturn"(%1) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>
// MONO: }
// ERASE: builtin.module {
// ERASE:   %0 = "tlam.tlambda"() ({
// ERASE:     %1 = "test.op"() : () -> !tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>
// ERASE:     "tlam.treturn"(%1) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
// ERASE:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// ERASE: }
// LOWER: builtin.module {
// LOWER:   %0 = "tlam.tlambda"() ({
// LOWER:     %1 = "test.op"() : () -> !tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>
// LOWER:     "tlam.treturn"(%1) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
// LOWER:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// LOWER: }
// FULL: builtin.module {
// FULL:   %0 = "tlam.tlambda"() ({
// FULL:     %1 = "test.op"() : () -> !tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>
// FULL:     "tlam.treturn"(%1) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
// FULL:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// FULL: }
// FULL2: builtin.module {
// FULL2:   %0 = "tlam.tlambda"() ({
// FULL2:     %1 = "test.op"() : () -> !tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>
// FULL2:     "tlam.treturn"(%1) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
// FULL2:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// FULL2: }

// -----

// VALID: duplicate specializations; one should be removable by canonicalize/cse.
builtin.module {
  %0 = "tlam.tlambda"() ({
    %poly_id = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
    %a = "tlam.tapply"(%poly_id) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    %b = "tlam.tapply"(%poly_id) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%a) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam.tlambda"() ({
// VERIFY:     %1 = "tlam.tlambda"() ({
// VERIFY:       %2 = "tlam.vlambda"() ({
// VERIFY:       ^bb0(%3: !tlam.bvar<0>):
// VERIFY:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// VERIFY:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// VERIFY:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// VERIFY:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// VERIFY:     %2 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// VERIFY:     %3 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// VERIFY:     "tlam.treturn"(%2) : (!tlam.fun<i64, i64>) -> ()
// VERIFY:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam.tlambda"() ({
// BETA:     %1 = "tlam.tlambda"() ({
// BETA:       %2 = "tlam.vlambda"() ({
// BETA:       ^bb0(%3: !tlam.bvar<0>):
// BETA:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// BETA:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// BETA:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// BETA:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// BETA:     %2 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// BETA:     %3 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// BETA:     "tlam.treturn"(%2) : (!tlam.fun<i64, i64>) -> ()
// BETA:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:     %1 = "tlam.tlambda"() ({
// MONO:       %2 = "tlam.vlambda"() ({
// MONO:       ^bb0(%3: !tlam.bvar<0>):
// MONO:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// MONO:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// MONO:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// MONO:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// MONO:     %2 = "tlam.vlambda"() ({
// MONO:     ^bb0(%3: i64):
// MONO:       "tlam.vreturn"(%3) : (i64) -> ()
// MONO:     }) : () -> !tlam.fun<i64, i64>
// MONO:     "tlam.treturn"(%2) : (!tlam.fun<i64, i64>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// MONO: }
// ERASE: builtin.module {
// ERASE:   %0 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%1: !tlam.bvar<0>):
// ERASE:     "tlam.vreturn"(%1) : (!tlam.bvar<0>) -> ()
// ERASE:   }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// ERASE:   %1 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%2: i64):
// ERASE:     "tlam.vreturn"(%2) : (i64) -> ()
// ERASE:   }) : () -> !tlam.fun<i64, i64>
// ERASE: }
// LOWER: builtin.module {
// LOWER:   func.func @lifted_2(%0: i64) -> i64 {
// LOWER:     func.return %0 : i64
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_2 : (i64) -> i64
// LOWER:   func.func @lifted_1(%1: !tlam.bvar<0>) -> !tlam.bvar<0> {
// LOWER:     func.return %1 : !tlam.bvar<0>
// LOWER:   }
// LOWER:   %1 = func.constant @lifted_1 : (!tlam.bvar<0>) -> !tlam.bvar<0>
// LOWER: }
// FULL: builtin.module {
// FULL:   func.func @lifted_1(%0: i64) -> i64 {
// FULL:     func.return %0 : i64
// FULL:   }
// FULL:   %0 = func.constant @lifted_1 : (i64) -> i64
// FULL: }
// FULL2: builtin.module {
// FULL2:   func.func @lifted_1(%0: i64) -> i64 {
// FULL2:     func.return %0 : i64
// FULL2:   }
// FULL2:   %0 = func.constant @lifted_1 : (i64) -> i64
// FULL2: }

// -----

// VALID: full pipeline is stable when run twice.
builtin.module {
  %outer = "tlam.tlambda"() ({
    %poly = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
    %spec = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    %c = "arith.constant"() <{value = 3 : i64}> : () -> (i64)
    %r = "tlam.vapply"(%spec, %c) : (!tlam.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()
    %id2 = "tlam.vlambda"() ({
    ^bb0(%z: i64):
      "tlam.vreturn"(%z) : (i64) -> ()
    }) : () -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%id2) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam.tlambda"() ({
// VERIFY:     %1 = "tlam.tlambda"() ({
// VERIFY:       %2 = "tlam.vlambda"() ({
// VERIFY:       ^bb0(%3: !tlam.bvar<0>):
// VERIFY:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// VERIFY:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// VERIFY:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// VERIFY:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// VERIFY:     %2 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// VERIFY:     %3 = "arith.constant"() <{value = 3}> : () -> i64
// VERIFY:     %4 = "tlam.vapply"(%2, %3) : (!tlam.fun<i64, i64>, i64) -> i64
// VERIFY:     "test.use"(%4) : (i64) -> ()
// VERIFY:     %5 = "tlam.vlambda"() ({
// VERIFY:     ^bb0(%6: i64):
// VERIFY:       "tlam.vreturn"(%6) : (i64) -> ()
// VERIFY:     }) : () -> !tlam.fun<i64, i64>
// VERIFY:     "tlam.treturn"(%5) : (!tlam.fun<i64, i64>) -> ()
// VERIFY:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam.tlambda"() ({
// BETA:     %1 = "tlam.tlambda"() ({
// BETA:       %2 = "tlam.vlambda"() ({
// BETA:       ^bb0(%3: !tlam.bvar<0>):
// BETA:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// BETA:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// BETA:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// BETA:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// BETA:     %2 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// BETA:     %3 = "arith.constant"() <{value = 3}> : () -> i64
// BETA:     %4 = "tlam.vapply"(%2, %3) : (!tlam.fun<i64, i64>, i64) -> i64
// BETA:     "test.use"(%4) : (i64) -> ()
// BETA:     %5 = "tlam.vlambda"() ({
// BETA:     ^bb0(%6: i64):
// BETA:       "tlam.vreturn"(%6) : (i64) -> ()
// BETA:     }) : () -> !tlam.fun<i64, i64>
// BETA:     "tlam.treturn"(%5) : (!tlam.fun<i64, i64>) -> ()
// BETA:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:     %1 = "tlam.vlambda"() ({
// MONO:     ^bb0(%2: i64):
// MONO:       "tlam.vreturn"(%2) : (i64) -> ()
// MONO:     }) : () -> !tlam.fun<i64, i64>
// MONO:     %2 = "arith.constant"() <{value = 3}> : () -> i64
// MONO:     %3 = "tlam.vapply"(%1, %2) : (!tlam.fun<i64, i64>, i64) -> i64
// MONO:     "test.use"(%3) : (i64) -> ()
// MONO:     %4 = "tlam.vlambda"() ({
// MONO:     ^bb0(%5: i64):
// MONO:       "tlam.vreturn"(%5) : (i64) -> ()
// MONO:     }) : () -> !tlam.fun<i64, i64>
// MONO:     "tlam.treturn"(%4) : (!tlam.fun<i64, i64>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// MONO: }
// ERASE: builtin.module {
// ERASE:   %0 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%1: i64):
// ERASE:     "tlam.vreturn"(%1) : (i64) -> ()
// ERASE:   }) : () -> !tlam.fun<i64, i64>
// ERASE:   %1 = "arith.constant"() <{value = 3}> : () -> i64
// ERASE:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i64, i64>, i64) -> i64
// ERASE:   "test.use"(%2) : (i64) -> ()
// ERASE:   %3 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%4: i64):
// ERASE:     "tlam.vreturn"(%4) : (i64) -> ()
// ERASE:   }) : () -> !tlam.fun<i64, i64>
// ERASE: }
// LOWER: builtin.module {
// LOWER:   func.func @lifted_2(%0: i64) -> i64 {
// LOWER:     func.return %0 : i64
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_2 : (i64) -> i64
// LOWER:   func.func @lifted_1(%1: i64) -> i64 {
// LOWER:     func.return %1 : i64
// LOWER:   }
// LOWER:   %1 = func.constant @lifted_1 : (i64) -> i64
// LOWER:   %2 = "arith.constant"() <{value = 3}> : () -> i64
// LOWER:   %3 = "func.call_indirect"(%1, %2) : ((i64) -> i64, i64) -> i64
// LOWER:   "test.use"(%3) : (i64) -> ()
// LOWER: }
// FULL: builtin.module {
// FULL:   func.func @lifted_2(%0: i64) -> i64 {
// FULL:     func.return %0 : i64
// FULL:   }
// FULL:   %0 = func.constant @lifted_2 : (i64) -> i64
// FULL:   func.func @lifted_1(%1: i64) -> i64 {
// FULL:     func.return %1 : i64
// FULL:   }
// FULL:   %1 = func.constant @lifted_1 : (i64) -> i64
// FULL:   %2 = "arith.constant"() <{value = 3}> : () -> i64
// FULL:   %3 = "func.call_indirect"(%1, %2) : ((i64) -> i64, i64) -> i64
// FULL:   "test.use"(%3) : (i64) -> ()
// FULL: }
// FULL2: builtin.module {
// FULL2:   func.func @lifted_2(%0: i64) -> i64 {
// FULL2:     func.return %0 : i64
// FULL2:   }
// FULL2:   %0 = func.constant @lifted_2 : (i64) -> i64
// FULL2:   func.func @lifted_1(%1: i64) -> i64 {
// FULL2:     func.return %1 : i64
// FULL2:   }
// FULL2:   %1 = func.constant @lifted_1 : (i64) -> i64
// FULL2:   %2 = "arith.constant"() <{value = 3}> : () -> i64
// FULL2:   %3 = "func.call_indirect"(%1, %2) : ((i64) -> i64, i64) -> i64
// FULL2:   "test.use"(%3) : (i64) -> ()
// FULL2: }

// -----

// INVALID: tlambda terminator is not last; all pass entrypoints should reject.
builtin.module {
  %bad = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    "tlam.treturn"(%id) : (!tlam.fun<i32, i32>) -> ()
    "test.op"() : () -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i32, i32>>)
}
// VERIFY: tlambda: last op must be tlam.treturn, got 'test.op'
// BETA: tlambda: last op must be tlam.treturn, got 'test.op'
// MONO: tlambda: last op must be tlam.treturn, got 'test.op'
// ERASE: tlambda: last op must be tlam.treturn, got 'test.op'
// LOWER: tlambda: last op must be tlam.treturn, got 'test.op'
// FULL: tlambda: last op must be tlam.treturn, got 'test.op'
// FULL2: tlambda: last op must be tlam.treturn, got 'test.op'

// -----

// INVALID: vlambda terminator is not last; all pass entrypoints should reject.
builtin.module {
  %bad = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
    "test.op"() : () -> ()
  }) : () -> (!tlam.fun<i32, i32>)
}
// VERIFY: vlambda: last op must be tlam.vreturn, got 'test.op'
// BETA: vlambda: last op must be tlam.vreturn, got 'test.op'
// MONO: vlambda: last op must be tlam.vreturn, got 'test.op'
// ERASE: vlambda: last op must be tlam.vreturn, got 'test.op'
// LOWER: vlambda: last op must be tlam.vreturn, got 'test.op'
// FULL: vlambda: last op must be tlam.vreturn, got 'test.op'
// FULL2: vlambda: last op must be tlam.vreturn, got 'test.op'

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam-de-bruijn | filecheck %s --check-prefix=BETA -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn | filecheck %s --check-prefix=MONO -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn | filecheck %s --check-prefix=ERASE -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func | filecheck %s --check-prefix=LOWER -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam-de-bruijn,canonicalize,cse,monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,canonicalize,cse | filecheck %s --check-prefix=FULL -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam-de-bruijn,canonicalize,cse,monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,canonicalize,cse,beta-reduce-tlam-de-bruijn,canonicalize,cse,monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,canonicalize,cse | filecheck %s --check-prefix=FULL2 -DFILE=%s

// VALID: polymorphic program that monomorphizes, erases TLam, then lowers.
builtin.module {
  %outer = "tlam_dbi.tlambda"() ({
    %poly_id = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

    %spec = "tlam_dbi.tapply"(%poly_id) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    "tlam_dbi.treturn"(%spec) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
  %top = "tlam_dbi.tapply"(%outer) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>) -> (!tlam_dbi.fun<i64, i64>)
  "test.use"(%top) : (!tlam_dbi.fun<i64, i64>) -> ()
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam_dbi.tlambda"() ({
// VERIFY:     %1 = "tlam_dbi.tlambda"() ({
// VERIFY:       %2 = "tlam_dbi.vlambda"() ({
// VERIFY:       ^bb0(%3: !tlam_dbi.bvar<0>):
// VERIFY:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// VERIFY:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// VERIFY:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// VERIFY:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// VERIFY:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
// VERIFY:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// VERIFY:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam_dbi.tlambda"() ({
// BETA:     %1 = "tlam_dbi.tlambda"() ({
// BETA:       %2 = "tlam_dbi.vlambda"() ({
// BETA:       ^bb0(%3: !tlam_dbi.bvar<0>):
// BETA:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// BETA:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// BETA:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// BETA:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// BETA:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
// BETA:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// BETA:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam_dbi.tlambda"() ({
// MONO:     %1 = "tlam_dbi.tlambda"() ({
// MONO:       %2 = "tlam_dbi.vlambda"() ({
// MONO:       ^bb0(%3: !tlam_dbi.bvar<0>):
// MONO:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// MONO:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// MONO:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// MONO:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// MONO:     %2 = "tlam_dbi.vlambda"() ({
// MONO:     ^bb0(%3: i64):
// MONO:       "tlam_dbi.vreturn"(%3) : (i64) -> ()
// MONO:     }) : () -> !tlam_dbi.fun<i64, i64>
// MONO:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// MONO:   %1 = "tlam_dbi.tlambda"() ({
// MONO:     %2 = "tlam_dbi.vlambda"() ({
// MONO:     ^bb0(%3: !tlam_dbi.bvar<0>):
// MONO:       "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// MONO:     }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// MONO:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// MONO:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// MONO:   %2 = "tlam_dbi.vlambda"() ({
// MONO:   ^bb0(%3: i64):
// MONO:     "tlam_dbi.vreturn"(%3) : (i64) -> ()
// MONO:   }) : () -> !tlam_dbi.fun<i64, i64>
// MONO:   "test.use"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO: }
// ERASE: builtin.module {
// ERASE:   %0 = "tlam_dbi.vlambda"() ({
// ERASE:   ^bb0(%1: i64):
// ERASE:     "tlam_dbi.vreturn"(%1) : (i64) -> ()
// ERASE:   }) : () -> !tlam_dbi.fun<i64, i64>
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
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %x = "test.op"() : () -> (i32)
  %r = "tlam_dbi.vapply"(%id, %x) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam_dbi.vlambda"() ({
// VERIFY:   ^bb0(%1: i32):
// VERIFY:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// VERIFY:   }) : () -> !tlam_dbi.fun<i32, i32>
// VERIFY:   %1 = "test.op"() : () -> i32
// VERIFY:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam_dbi.vlambda"() ({
// BETA:   ^bb0(%1: i32):
// BETA:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// BETA:   }) : () -> !tlam_dbi.fun<i32, i32>
// BETA:   %1 = "test.op"() : () -> i32
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam_dbi.vlambda"() ({
// MONO:   ^bb0(%1: i32):
// MONO:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// MONO:   }) : () -> !tlam_dbi.fun<i32, i32>
// MONO:   %1 = "test.op"() : () -> i32
// MONO:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// MONO: }
// ERASE: builtin.module {
// ERASE:   %0 = "tlam_dbi.vlambda"() ({
// ERASE:   ^bb0(%1: i32):
// ERASE:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// ERASE:   }) : () -> !tlam_dbi.fun<i32, i32>
// ERASE:   %1 = "test.op"() : () -> i32
// ERASE:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
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
// FULL:   %0 = "test.op"() : () -> i32
// FULL: }
// FULL2: builtin.module {
// FULL2:   %0 = "test.op"() : () -> i32
// FULL2: }

// -----

// VALID: beta-reduce removes direct vapply(vlambda, arg).
builtin.module {
  "test.pipeline.beta"() : () -> ()
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %x = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %r = "tlam_dbi.vapply"(%id, %x) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%r) : (i32) -> ()
}
// VERIFY: builtin.module {
// VERIFY:   "test.pipeline.beta"() : () -> ()
// VERIFY:   %0 = "tlam_dbi.vlambda"() ({
// VERIFY:   ^bb0(%1: i32):
// VERIFY:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// VERIFY:   }) : () -> !tlam_dbi.fun<i32, i32>
// VERIFY:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// VERIFY:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// VERIFY:   "test.use"(%2) : (i32) -> ()
// VERIFY: }
// BETA: builtin.module {
// BETA:   "test.pipeline.beta"() : () -> ()
// BETA:   %0 = "tlam_dbi.vlambda"() ({
// BETA:   ^bb0(%1: i32):
// BETA:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// BETA:   }) : () -> !tlam_dbi.fun<i32, i32>
// BETA:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// BETA:   "test.use"(%1) : (i32) -> ()
// BETA: }
// MONO: builtin.module {
// MONO:   "test.pipeline.beta"() : () -> ()
// MONO:   %0 = "tlam_dbi.vlambda"() ({
// MONO:   ^bb0(%1: i32):
// MONO:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// MONO:   }) : () -> !tlam_dbi.fun<i32, i32>
// MONO:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// MONO:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// MONO:   "test.use"(%2) : (i32) -> ()
// MONO: }
// ERASE: builtin.module {
// ERASE:   "test.pipeline.beta"() : () -> ()
// ERASE:   %0 = "tlam_dbi.vlambda"() ({
// ERASE:   ^bb0(%1: i32):
// ERASE:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// ERASE:   }) : () -> !tlam_dbi.fun<i32, i32>
// ERASE:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// ERASE:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
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
// FULL:   "test.pipeline.beta"() : () -> ()
// FULL:   %0 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// FULL:   "test.use"(%0) : (i32) -> ()
// FULL: }
// FULL2: builtin.module {
// FULL2:   "test.pipeline.beta"() : () -> ()
// FULL2:   %0 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// FULL2:   "test.use"(%0) : (i32) -> ()
// FULL2: }

// -----

// INVALID: DBI out-of-bounds bvar in type.
builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<1>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<1>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>>)
}
// VERIFY: debruijn-dbi: bvar<1> out of scope at depth=1
// BETA: debruijn-dbi: bvar<1> out of scope at depth=1
// MONO: debruijn-dbi: bvar<1> out of scope at depth=1
// ERASE: debruijn-dbi: bvar<1> out of scope at depth=1
// LOWER: debruijn-dbi: bvar<1> out of scope at depth=1
// FULL: debruijn-dbi: bvar<1> out of scope at depth=1
// FULL2: debruijn-dbi: bvar<1> out of scope at depth=1

// -----

// INVALID: TLambda has one block arg (must be zero).
builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
  ^bb0(%a: !tlam_dbi.type):
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: i32):
      "tlam_dbi.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<i32, i32>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>)
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
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "test.op"() : () -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
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
  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %bad = "tlam_dbi.tapply"(%id) <{tyArg = i32}> : (!tlam_dbi.fun<i32, i32>) -> (i32)
}
// VERIFY: tapply: expected operand of type tlam.forall, got !tlam_dbi.fun<i32, i32>
// BETA: tapply: expected operand of type tlam.forall, got !tlam_dbi.fun<i32, i32>
// MONO: tapply: expected operand of type tlam.forall, got !tlam_dbi.fun<i32, i32>
// ERASE: tapply: expected operand of type tlam.forall, got !tlam_dbi.fun<i32, i32>
// LOWER: tapply: expected operand of type tlam.forall, got !tlam_dbi.fun<i32, i32>
// FULL: tapply: expected operand of type tlam.forall, got !tlam_dbi.fun<i32, i32>
// FULL2: tapply: expected operand of type tlam.forall, got !tlam_dbi.fun<i32, i32>

// -----

// INVALID: tapply annotated result type is not instantiate(forall, arg).
builtin.module {
  %poly = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
  %bad = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i32>)
}
// VERIFY: tapply: result !tlam_dbi.fun<i64, i32> != instantiated !tlam_dbi.fun<i64, i64>
// BETA: tapply: result !tlam_dbi.fun<i64, i32> != instantiated !tlam_dbi.fun<i64, i64>
// MONO: tapply: result !tlam_dbi.fun<i64, i32> != instantiated !tlam_dbi.fun<i64, i64>
// ERASE: tapply: result !tlam_dbi.fun<i64, i32> != instantiated !tlam_dbi.fun<i64, i64>
// LOWER: tapply: result !tlam_dbi.fun<i64, i32> != instantiated !tlam_dbi.fun<i64, i64>
// FULL: tapply: result !tlam_dbi.fun<i64, i32> != instantiated !tlam_dbi.fun<i64, i64>
// FULL2: tapply: result !tlam_dbi.fun<i64, i32> != instantiated !tlam_dbi.fun<i64, i64>

// -----

// INVALID: tapply type argument is not a TypeAttribute.
builtin.module {
  %poly = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
  %bad = "tlam_dbi.tapply"(%poly) <{tyArg = "oops"}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i32, i32>)
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
  %0 = "tlam_dbi.tlambda"() ({
    %g = "tlam_dbi.tlambda"() ({
      %h = "tlam_dbi.tlambda"() ({
        %u = "test.op"() : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>)
        "tlam_dbi.treturn"(%u) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>) -> ()
      }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>)
      "tlam_dbi.treturn"(%h) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>)
    %spec = "tlam_dbi.tapply"(%g) <{tyArg = !tlam_dbi.bvar<0>}> : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>) -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>)
    "tlam_dbi.treturn"(%spec) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam_dbi.tlambda"() ({
// VERIFY:     %1 = "tlam_dbi.tlambda"() ({
// VERIFY:       %2 = "tlam_dbi.tlambda"() ({
// VERIFY:         %3 = "test.op"() : () -> !tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>
// VERIFY:         "tlam_dbi.treturn"(%3) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>) -> ()
// VERIFY:       }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// VERIFY:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// VERIFY:     }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// VERIFY:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = !tlam_dbi.bvar<0>}> : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>) -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// VERIFY:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// VERIFY:   }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam_dbi.tlambda"() ({
// BETA:     %1 = "tlam_dbi.tlambda"() ({
// BETA:       %2 = "tlam_dbi.tlambda"() ({
// BETA:         %3 = "test.op"() : () -> !tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>
// BETA:         "tlam_dbi.treturn"(%3) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>) -> ()
// BETA:       }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// BETA:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// BETA:     }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// BETA:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = !tlam_dbi.bvar<0>}> : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>) -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// BETA:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// BETA:   }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam_dbi.tlambda"() ({
// MONO:     %1 = "tlam_dbi.tlambda"() ({
// MONO:       %2 = "tlam_dbi.tlambda"() ({
// MONO:         %3 = "test.op"() : () -> !tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>
// MONO:         "tlam_dbi.treturn"(%3) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>) -> ()
// MONO:       }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// MONO:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// MONO:     }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// MONO:     %2 = "tlam_dbi.tlambda"() ({
// MONO:       %3 = "test.op"() : () -> !tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>
// MONO:       "tlam_dbi.treturn"(%3) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>) -> ()
// MONO:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// MONO:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// MONO:   }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// MONO: }
// ERASE: builtin.module {
// ERASE: ^bb0:
// ERASE: }
// LOWER: builtin.module {
// LOWER: ^bb0:
// LOWER: }
// FULL: builtin.module {
// FULL: ^bb0:
// FULL: }
// FULL2: builtin.module {
// FULL2: ^bb0:
// FULL2: }

// -----

// VALID: duplicate specializations; one should be removable by canonicalize/cse.
builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
    %poly_id = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
    %a = "tlam_dbi.tapply"(%poly_id) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    %b = "tlam_dbi.tapply"(%poly_id) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    "tlam_dbi.treturn"(%a) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam_dbi.tlambda"() ({
// VERIFY:     %1 = "tlam_dbi.tlambda"() ({
// VERIFY:       %2 = "tlam_dbi.vlambda"() ({
// VERIFY:       ^bb0(%3: !tlam_dbi.bvar<0>):
// VERIFY:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// VERIFY:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// VERIFY:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// VERIFY:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// VERIFY:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
// VERIFY:     %3 = "tlam_dbi.tapply"(%1) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
// VERIFY:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// VERIFY:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam_dbi.tlambda"() ({
// BETA:     %1 = "tlam_dbi.tlambda"() ({
// BETA:       %2 = "tlam_dbi.vlambda"() ({
// BETA:       ^bb0(%3: !tlam_dbi.bvar<0>):
// BETA:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// BETA:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// BETA:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// BETA:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// BETA:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
// BETA:     %3 = "tlam_dbi.tapply"(%1) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
// BETA:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// BETA:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam_dbi.tlambda"() ({
// MONO:     %1 = "tlam_dbi.tlambda"() ({
// MONO:       %2 = "tlam_dbi.vlambda"() ({
// MONO:       ^bb0(%3: !tlam_dbi.bvar<0>):
// MONO:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// MONO:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// MONO:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// MONO:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// MONO:     %2 = "tlam_dbi.vlambda"() ({
// MONO:     ^bb0(%3: i64):
// MONO:       "tlam_dbi.vreturn"(%3) : (i64) -> ()
// MONO:     }) : () -> !tlam_dbi.fun<i64, i64>
// MONO:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// MONO: }
// ERASE: builtin.module {
// ERASE: ^bb0:
// ERASE: }
// LOWER: builtin.module {
// LOWER: ^bb0:
// LOWER: }
// FULL: builtin.module {
// FULL: ^bb0:
// FULL: }
// FULL2: builtin.module {
// FULL2: ^bb0:
// FULL2: }

// -----

// VALID: full pipeline is stable when run twice.
builtin.module {
  %outer = "tlam_dbi.tlambda"() ({
    %poly = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
    %spec = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    %c = "arith.constant"() <{value = 3 : i64}> : () -> (i64)
    %r = "tlam_dbi.vapply"(%spec, %c) : (!tlam_dbi.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()
    %id2 = "tlam_dbi.vlambda"() ({
    ^bb0(%z: i64):
      "tlam_dbi.vreturn"(%z) : (i64) -> ()
    }) : () -> (!tlam_dbi.fun<i64, i64>)
    "tlam_dbi.treturn"(%id2) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam_dbi.tlambda"() ({
// VERIFY:     %1 = "tlam_dbi.tlambda"() ({
// VERIFY:       %2 = "tlam_dbi.vlambda"() ({
// VERIFY:       ^bb0(%3: !tlam_dbi.bvar<0>):
// VERIFY:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// VERIFY:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// VERIFY:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// VERIFY:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// VERIFY:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
// VERIFY:     %3 = "arith.constant"() <{value = 3}> : () -> i64
// VERIFY:     %4 = "tlam_dbi.vapply"(%2, %3) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
// VERIFY:     "test.use"(%4) : (i64) -> ()
// VERIFY:     %5 = "tlam_dbi.vlambda"() ({
// VERIFY:     ^bb0(%6: i64):
// VERIFY:       "tlam_dbi.vreturn"(%6) : (i64) -> ()
// VERIFY:     }) : () -> !tlam_dbi.fun<i64, i64>
// VERIFY:     "tlam_dbi.treturn"(%5) : (!tlam_dbi.fun<i64, i64>) -> ()
// VERIFY:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam_dbi.tlambda"() ({
// BETA:     %1 = "tlam_dbi.tlambda"() ({
// BETA:       %2 = "tlam_dbi.vlambda"() ({
// BETA:       ^bb0(%3: !tlam_dbi.bvar<0>):
// BETA:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// BETA:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// BETA:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// BETA:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// BETA:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
// BETA:     %3 = "arith.constant"() <{value = 3}> : () -> i64
// BETA:     %4 = "tlam_dbi.vapply"(%2, %3) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
// BETA:     "test.use"(%4) : (i64) -> ()
// BETA:     %5 = "tlam_dbi.vlambda"() ({
// BETA:     ^bb0(%6: i64):
// BETA:       "tlam_dbi.vreturn"(%6) : (i64) -> ()
// BETA:     }) : () -> !tlam_dbi.fun<i64, i64>
// BETA:     "tlam_dbi.treturn"(%5) : (!tlam_dbi.fun<i64, i64>) -> ()
// BETA:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam_dbi.tlambda"() ({
// MONO:     %1 = "tlam_dbi.tlambda"() ({
// MONO:       %2 = "tlam_dbi.vlambda"() ({
// MONO:       ^bb0(%3: !tlam_dbi.bvar<0>):
// MONO:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// MONO:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// MONO:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// MONO:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// MONO:     %2 = "tlam_dbi.vlambda"() ({
// MONO:     ^bb0(%3: i64):
// MONO:       "tlam_dbi.vreturn"(%3) : (i64) -> ()
// MONO:     }) : () -> !tlam_dbi.fun<i64, i64>
// MONO:     %3 = "arith.constant"() <{value = 3}> : () -> i64
// MONO:     %4 = "tlam_dbi.vapply"(%2, %3) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
// MONO:     "test.use"(%4) : (i64) -> ()
// MONO:     %5 = "tlam_dbi.vlambda"() ({
// MONO:     ^bb0(%6: i64):
// MONO:       "tlam_dbi.vreturn"(%6) : (i64) -> ()
// MONO:     }) : () -> !tlam_dbi.fun<i64, i64>
// MONO:     "tlam_dbi.treturn"(%5) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// MONO: }
// ERASE: builtin.module {
// ERASE: ^bb0:
// ERASE: }
// LOWER: builtin.module {
// LOWER: ^bb0:
// LOWER: }
// FULL: builtin.module {
// FULL: ^bb0:
// FULL: }
// FULL2: builtin.module {
// FULL2: ^bb0:
// FULL2: }

// -----

// INVALID: tlambda terminator is not last; all pass entrypoints should reject.
builtin.module {
  %bad = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: i32):
      "tlam_dbi.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<i32, i32>) -> ()
    "test.op"() : () -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>)
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
  %bad = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
    "test.op"() : () -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
}
// VERIFY: vlambda: last op must be tlam.vreturn, got 'test.op'
// BETA: vlambda: last op must be tlam.vreturn, got 'test.op'
// MONO: vlambda: last op must be tlam.vreturn, got 'test.op'
// ERASE: vlambda: last op must be tlam.vreturn, got 'test.op'
// LOWER: vlambda: last op must be tlam.vreturn, got 'test.op'
// FULL: vlambda: last op must be tlam.vreturn, got 'test.op'
// FULL2: vlambda: last op must be tlam.vreturn, got 'test.op'

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
// VERIFY: builtin.module
// VERIFY: "tlam.tapply"
// BETA: builtin.module
// BETA: "tlam.tapply"
// MONO: builtin.module
// MONO-NOT: "tlam.tapply"(
// MONO: !tlam.fun<i64, i64>
// ERASE: builtin.module
// ERASE-NOT: "tlam.tlambda"
// ERASE-NOT: "tlam.treturn"
// ERASE: "tlam.vlambda"
// LOWER: builtin.module
// LOWER: func.func
// LOWER: func.return
// FULL: builtin.module
// FULL: func.func
// FULL: func.return

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
// VERIFY: "tlam.vapply"
// LOWER: "func.call_indirect"
// FULL: builtin.module

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
// BETA-LABEL: "test.pipeline.beta"() : () -> ()
// BETA-NOT: "tlam.vapply"
// BETA: "test.use"

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

// -----

// INVALID: missing VReturn terminator in VLambda.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "test.op"() : () -> ()
  }) : () -> (!tlam.fun<i32, i32>)
}
// VERIFY: vlambda: last op must be tlam.vreturn
// BETA: vlambda: last op must be tlam.vreturn
// MONO: vlambda: last op must be tlam.vreturn

// -----

// INVALID: tapply operand is not forall.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %bad = "tlam.tapply"(%id) <{tyArg = i32}> : (!tlam.fun<i32, i32>) -> (i32)
}
// VERIFY: tapply: expected operand of type tlam.forall
// BETA: tapply: expected operand of type tlam.forall
// MONO: tapply: expected operand of type tlam.forall

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
// VERIFY: tapply: result
// VERIFY: instantiated
// BETA: tapply: result
// BETA: instantiated
// MONO: tapply: result
// MONO: instantiated

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
// MONO: !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// FULL: func.func

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
// MONO-NOT: "tlam.tapply"(
// FULL: func.func

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
// FULL-NOT: "tlam."
// FULL: "func.call_indirect"
// FULL2-NOT: "tlam."
// FULL2: "func.call_indirect"

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
// VERIFY: tlambda: last op must be tlam.treturn
// BETA: tlambda: last op must be tlam.treturn
// MONO: tlambda: last op must be tlam.treturn
// ERASE: tlambda: last op must be tlam.treturn
// LOWER: tlambda: last op must be tlam.treturn
// FULL: tlambda: last op must be tlam.treturn
// FULL2: tlambda: last op must be tlam.treturn

// -----

// INVALID: vlambda terminator is not last; all pass entrypoints should reject.
builtin.module {
  %bad = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
    "test.op"() : () -> ()
  }) : () -> (!tlam.fun<i32, i32>)
}
// VERIFY: vlambda: last op must be tlam.vreturn
// BETA: vlambda: last op must be tlam.vreturn
// MONO: vlambda: last op must be tlam.vreturn
// ERASE: vlambda: last op must be tlam.vreturn
// LOWER: vlambda: last op must be tlam.vreturn
// FULL: vlambda: last op must be tlam.vreturn
// FULL2: vlambda: last op must be tlam.vreturn

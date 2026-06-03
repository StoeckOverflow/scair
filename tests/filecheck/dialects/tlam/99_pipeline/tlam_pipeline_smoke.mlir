// Purpose: End-to-end TLam pipeline smoke checks over key pass combinations.
// Invariants covered: Monomorphize/erase/lower/full pipeline success plus invalid-input verifier failures.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=MONO
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize,dce,erase-tlam,lower-tlam-to-func --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=LOWER
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam,canonicalize,cse,canonicalize,monomorphize,dce,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=BETAFULL
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize,monomorphize,beta-reduce-tlam,dce,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=BETALATE

// Targets: end-to-end SSA-in-types TLam pipeline safety, including verifier-fail
// behavior on invalid input and pass-order regressions with beta-reduction.

// Positive: polymorphic identity through monomorphize/erase/lower/full-pipeline.
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: !value<%T>):
      "tlam.vreturn"(%x) : (!value<%T>) -> ()
    }) : () -> !tlam.fun<!value<%T>, !value<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %id_i64 = "tlam.tapply"(%mk) <{tyArg = i64}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
       -> !tlam.fun<i64, i64>
}

// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:   ^bb0(%1: !tlam.type):
// MONO:     %2 = "tlam.vlambda"() ({
// MONO:     ^bb1(%3: !value<%1>):
// MONO:       "tlam.vreturn"(%3) : (!value<%1>) -> ()
// MONO:     }) : () -> !tlam.fun<!value<%1>, !value<%1>>
// MONO:     "tlam.treturn"(%2) : (!tlam.fun<!value<%1>, !value<%1>>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// MONO:   %1 = "tlam.vlambda"() ({
// MONO:   ^bb0(%2: i64):
// MONO:     "tlam.vreturn"(%2) : (i64) -> ()
// MONO:   }) : () -> !tlam.fun<i64, i64>
// MONO: }

// LOWER: builtin.module {
// LOWER: ^bb0:
// LOWER: }

// -----

// Negative pipeline: non-dominating tvar in tapply tyArg (must report, not crash).
builtin.module {
  %G = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %v = "builtin.unrealized_conversion_cast"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %h = "tlam.tapply"(%G) <{tyArg = !value<%T>}>
       : (!tlam.forall<i64>) -> i64

  %T = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
}

// MONO: ssa-dominance: value Value{{.*}} does not dominate its use in op `tlam.tapply`
// LOWER: ssa-dominance: value Value{{.*}} does not dominate its use in op `tlam.tapply`

// -----

// Negative pipeline: invalid DBI scoping (must report, not crash).
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.bvar<0>):
    "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
  }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
}

// MONO: debruijn: bvar<0> out of scope at depth=0
// LOWER: debruijn: bvar<0> out of scope at depth=0

// -----

// Pipeline with beta-reduce integrated before canonicalize.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 5 : i32}> : () -> i32
  %r = "tlam.vapply"(%id, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// BETAFULL: builtin.module {
// BETAFULL: ^bb0:
// BETAFULL: }

// BETALATE: builtin.module {
// BETALATE: ^bb0:
// BETALATE: }

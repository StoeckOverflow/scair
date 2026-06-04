// Purpose: End-to-end TLam pipeline smoke checks over key pass combinations.
// Invariants covered: Monomorphize/erase/lower/full pipeline success and beta pass-order smoke.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=MONO
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize,dce,erase-tlam,lower-tlam-to-func --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=LOWER
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam,canonicalize,cse,canonicalize,monomorphize,dce,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=BETAFULL
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize,monomorphize,beta-reduce-tlam,dce,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=BETALATE

// Targets: end-to-end SSA-in-types TLam pipeline safety and pass-order
// regressions with beta-reduction.

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

  %v = "arith.constant"() <{value = 5 : i64}> : () -> i64
  %r = "tlam.vapply"(%id_i64, %v) : (!tlam.fun<i64, i64>, i64) -> i64
  "test.use"(%r) : (i64) -> ()
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
// MONO:   %2 = "arith.constant"() <{value = 5}> : () -> i64
// MONO:   %3 = "tlam.vapply"(%1, %2) : (!tlam.fun<i64, i64>, i64) -> i64
// MONO:   "test.use"(%3) : (i64) -> ()
// MONO: }
// MONO: // -----
// MONO: builtin.module {
// MONO:   %0 = "tlam.vlambda"() ({
// MONO:   ^bb0(%1: i32):
// MONO:     "tlam.vreturn"(%1) : (i32) -> ()
// MONO:   }) : () -> !tlam.fun<i32, i32>
// MONO:   %1 = "arith.constant"() <{value = 5 : i32}> : () -> i32
// MONO:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// MONO: }

// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i64) -> i64 {
// LOWER:     func.return %0 : i64
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i64) -> i64
// LOWER:   %1 = "arith.constant"() <{value = 5}> : () -> i64
// LOWER:   %2 = "func.call_indirect"(%0, %1) : ((i64) -> i64, i64) -> i64
// LOWER:   "test.use"(%2) : (i64) -> ()
// LOWER: }
// LOWER: // -----
// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i32) -> i32 {
// LOWER:     func.return %0 : i32
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i32) -> i32
// LOWER:   %1 = "arith.constant"() <{value = 5 : i32}> : () -> i32
// LOWER:   %2 = "func.call_indirect"(%0, %1) : ((i32) -> i32, i32) -> i32
// LOWER: }

// -----

// Pipeline with beta-reduce integrated before and after monomorphize.
builtin.module {
  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 5 : i32}> : () -> i32
  %r = "tlam.vapply"(%id, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// BETAFULL: builtin.module {
// BETAFULL:   func.func @lifted_1(%0: i64) -> i64 {
// BETAFULL:     func.return %0 : i64
// BETAFULL:   }
// BETAFULL:   %0 = func.constant @lifted_1 : (i64) -> i64
// BETAFULL:   %1 = "arith.constant"() <{value = 5}> : () -> i64
// BETAFULL:   %2 = "func.call_indirect"(%0, %1) : ((i64) -> i64, i64) -> i64
// BETAFULL:   "test.use"(%2) : (i64) -> ()
// BETAFULL: }
// BETAFULL: // -----
// BETAFULL: builtin.module {
// BETAFULL: ^bb0:
// BETAFULL: }

// BETALATE: builtin.module {
// BETALATE:   %0 = "arith.constant"() <{value = 5}> : () -> i64
// BETALATE:   "test.use"(%0) : (i64) -> ()
// BETALATE: }
// BETALATE: // -----
// BETALATE: builtin.module {
// BETALATE: ^bb0:
// BETALATE: }

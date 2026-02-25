// Purpose: Full TLam pipeline stability and idempotence checks.
// Invariants covered: One-pass and two-pass full pipelines converge to stable lowered IR.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam,canonicalize,cse,canonicalize,monomorphize,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam,canonicalize,cse,canonicalize,monomorphize,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize,beta-reduce-tlam,canonicalize,cse,canonicalize,monomorphize,erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=PIPE

// Targets: pipeline idempotence (same stable lowered IR after one run or two).

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
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>

  %v = "arith.constant"() <{value = 42 : i64}> : () -> i64
  %r = "tlam.vapply"(%id_i64, %v) : (!tlam.fun<i64, i64>, i64) -> i64
  "test.use"(%r) : (i64) -> ()
}

// PIPE-LABEL: builtin.module {
// PIPE-NOT: "tlam.
// PIPE-NOT: !tlam.
// PIPE-NOT: "builtin.unrealized_conversion_cast"
// PIPE: func.func @lifted_{{[0-9]+}}(%{{[0-9]+}}: i64) -> i64 {
// PIPE: func.return %{{[0-9]+}} : i64
// PIPE: "func.call_indirect"
// PIPE: }

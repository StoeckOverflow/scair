// Purpose: Lowering completeness after erase/lower/reconcile pipeline stages.
// Invariants covered: No tlam ops/types/casts remain and lowered func IR verifies.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p erase-tlam,lower-tlam-to-func,reconcile-unrealized-casts,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=LOWER

// Targets: erase+lower+reconcile leaves no TLam ops/types/casts behind.

builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> !tlam.fun<i32, i32>
    "tlam.treturn"(%id) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<i32, i32>>

  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>
  %a = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
  "test.use"(%r) : (i32) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NOT: "tlam.
// LOWER-NOT: !tlam.
// LOWER-NOT: "builtin.unrealized_conversion_cast"
// LOWER: func.func @lifted_{{[0-9]+}}(%{{[0-9]+}}: i32) -> i32 {
// LOWER: func.return %{{[0-9]+}} : i32
// LOWER: "func.call_indirect"
// LOWER: }

// -----

// Nested placement: TLam constructs under execute_region are also erased/lowered.
builtin.module {
  "scf.execute_region"() ({
  ^bb0:
    %id = "tlam.vlambda"() ({
    ^bb1(%x: i64):
      "tlam.vreturn"(%x) : (i64) -> ()
    }) : () -> !tlam.fun<i64, i64>
    %v = "arith.constant"() <{value = 3 : i64}> : () -> i64
    %r = "tlam.vapply"(%id, %v) : (!tlam.fun<i64, i64>, i64) -> i64
    "test.use"(%r) : (i64) -> ()
    "scf.yield"() : () -> ()
  }) : () -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NOT: "tlam.
// LOWER-NOT: !tlam.
// LOWER-NOT: "builtin.unrealized_conversion_cast"
// LOWER: "scf.execute_region"() ({
// LOWER: "func.call_indirect"
// LOWER: "scf.yield"() : () -> ()
// LOWER: }

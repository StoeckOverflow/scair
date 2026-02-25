// Purpose: Regression tests for lower-tlam-to-func edge cases.
// Invariants covered: multi-use replacement safety.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p lower-tlam-to-func --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=LOWER

// Regression 1: same lambda value used multiple times by one user op should not crash.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  "test.consume2"(%f, %f) : (!tlam.fun<i32, i32>, !tlam.fun<i32, i32>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER: [[C:%[0-9]+]] = func.constant @lifted_{{[0-9]+}} : (i32) -> i32
// LOWER: "test.consume2"([[C]], [[C]]) : ((i32) -> i32, (i32) -> i32) -> ()
// LOWER-NOT: "tlam.vlambda"
// LOWER: }

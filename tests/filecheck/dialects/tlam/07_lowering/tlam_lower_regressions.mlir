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

// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i32) -> i32 {
// LOWER:     func.return %0 : i32
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i32) -> i32
// LOWER:   "test.consume2"(%0, %0) : ((i32) -> i32, (i32) -> i32) -> ()
// LOWER: }

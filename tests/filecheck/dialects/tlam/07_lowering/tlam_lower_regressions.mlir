// Purpose: Regression tests for lower-tlam-to-func edge cases.
// Invariants covered: multi-use replacement safety, type-use replacement, and body order.

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

// -----

// Regression 2: lambda replacement must update embedded type/attribute uses too.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  "test.use"(%f) {dep = !value<%f>} : (!tlam.fun<i32, i32>) -> ()
}

// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i32) -> i32 {
// LOWER:     func.return %0 : i32
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i32) -> i32
// LOWER:   "test.use"(%0) {dep = !value<%0>} : ((i32) -> i32) -> ()
// LOWER: }

// -----

// Regression 3: lifted lambda bodies preserve prefix ops and return order.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %c = "arith.constant"() <{value = 1 : i32}> : () -> i32
    %sum = "arith.addi"(%x, %c) : (i32, i32) -> i32
    "tlam.vreturn"(%sum) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 9 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
  "test.use"(%r) : (i32) -> ()
}

// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i32) -> i32 {
// LOWER:     %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// LOWER:     %2 = "arith.addi"(%0, %1) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
// LOWER:     func.return %2 : i32
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i32) -> i32
// LOWER:   %1 = "arith.constant"() <{value = 9 : i32}> : () -> i32
// LOWER:   %2 = "func.call_indirect"(%0, %1) : ((i32) -> i32, i32) -> i32
// LOWER:   "test.use"(%2) : (i32) -> ()
// LOWER: }

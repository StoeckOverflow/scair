// Purpose: Rank-0 policy + high-rank symbolic-shape coverage without duplicating existing mismatch suites.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

// Rank-0 tensors are valid: empty/fill/cast/add/mul.
builtin.module {
  %zero = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
  %e = "dtensor.empty"() : () -> !dtensor.tensor<[], f32>
  %f = "dtensor.fill"(%zero) : (f32) -> !dtensor.tensor<[], f32>
  %c = "dtensor.cast"(%e) : (!dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
  %s = "dtensor.add"(%e, %f)
    : (!dtensor.tensor<[], f32>, !dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
  %p = "dtensor.mul"(%s, %c)
    : (!dtensor.tensor<[], f32>, !dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
  "test.keep"(%p) : (!dtensor.tensor<[], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// VERIFY:   %1 = "dtensor.empty"() : () -> !dtensor.tensor<[], f32>
// VERIFY:   %2 = "dtensor.fill"(%0) : (f32) -> !dtensor.tensor<[], f32>
// VERIFY:   %3 = "dtensor.cast"(%1) : (!dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
// VERIFY:   %4 = "dtensor.add"(%1, %2) : (!dtensor.tensor<[], f32>, !dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
// VERIFY:   %5 = "dtensor.mul"(%4, %3) : (!dtensor.tensor<[], f32>, !dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
// VERIFY:   "test.keep"(%5) : (!dtensor.tensor<[], f32>) -> ()
// VERIFY: }
// CANON: builtin.module {
// CANON:   %0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// CANON:   %1 = "dtensor.empty"() : () -> !dtensor.tensor<[], f32>
// CANON:   %2 = "dtensor.fill"(%0) : (f32) -> !dtensor.tensor<[], f32>
// CANON:   %3 = "dtensor.cast"(%1) : (!dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
// CANON:   %4 = "dtensor.add"(%1, %2) : (!dtensor.tensor<[], f32>, !dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
// CANON:   %5 = "dtensor.mul"(%4, %3) : (!dtensor.tensor<[], f32>, !dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
// CANON:   "test.keep"(%5) : (!dtensor.tensor<[], f32>) -> ()
// CANON: }
// PIPE: builtin.module {
// PIPE:   %0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// PIPE:   %1 = "dtensor.empty"() : () -> !dtensor.tensor<[], f32>
// PIPE:   %2 = "dtensor.fill"(%0) : (f32) -> !dtensor.tensor<[], f32>
// PIPE:   %3 = "dtensor.cast"(%1) : (!dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
// PIPE:   %4 = "dtensor.add"(%1, %2) : (!dtensor.tensor<[], f32>, !dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
// PIPE:   %5 = "dtensor.mul"(%4, %3) : (!dtensor.tensor<[], f32>, !dtensor.tensor<[], f32>) -> !dtensor.tensor<[], f32>
// PIPE:   "test.keep"(%5) : (!dtensor.tensor<[], f32>) -> ()
// PIPE: }

// -----

// Rank-0 dim query must fail (axis out of bounds).
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %e = "dtensor.empty"() : () -> !dtensor.tensor<[], f32>
  // expected-error @below {{dtensor.dim: axis 0 out of bounds for rank 0}}
  %d = "dtensor.dim"(%e) <{axis = 0 : i32}> : (!dtensor.tensor<[], f32>) -> !value<%m>
}

// -----

// Valid high-rank (rank 5) with repeated symbolic dims.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "dtensor.empty"() : () -> !dtensor.tensor<[%n, %n, %m, %n, %n], f32>
  %b = "dtensor.empty"() : () -> !dtensor.tensor<[%n, %n, %m, %n, %n], f32>
  %s = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%n, %n, %m, %n, %n], f32>, !dtensor.tensor<[%n, %n, %m, %n, %n], f32>) -> !dtensor.tensor<[%n, %n, %m, %n, %n], f32>
  %p = "dtensor.mul"(%s, %a)
    : (!dtensor.tensor<[%n, %n, %m, %n, %n], f32>, !dtensor.tensor<[%n, %n, %m, %n, %n], f32>) -> !dtensor.tensor<[%n, %n, %m, %n, %n], f32>
  "test.keep"(%p) : (!dtensor.tensor<[%n, %n, %m, %n, %n], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %2 = "dtensor.empty"() : () -> !dtensor.tensor<[%1, %1, %0, %1, %1], f32>
// VERIFY:   %3 = "dtensor.empty"() : () -> !dtensor.tensor<[%1, %1, %0, %1, %1], f32>
// VERIFY:   %4 = "dtensor.add"(%2, %3) : (!dtensor.tensor<[%1, %1, %0, %1, %1], f32>, !dtensor.tensor<[%1, %1, %0, %1, %1], f32>) -> !dtensor.tensor<[%1, %1, %0, %1, %1], f32>
// VERIFY:   %5 = "dtensor.mul"(%4, %2) : (!dtensor.tensor<[%1, %1, %0, %1, %1], f32>, !dtensor.tensor<[%1, %1, %0, %1, %1], f32>) -> !dtensor.tensor<[%1, %1, %0, %1, %1], f32>
// VERIFY:   "test.keep"(%5) : (!dtensor.tensor<[%1, %1, %0, %1, %1], f32>) -> ()
// VERIFY: }

// -----

// Invalid high-rank add: semantically equal dims but SSA-distinct params.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %d0 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %d1 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "dtensor.empty"() : () -> !dtensor.tensor<[%d0, %d0, %d0, %d0], f32>
  %b = "dtensor.empty"() : () -> !dtensor.tensor<[%d1, %d1, %d1, %d1], f32>
  // expected-error @below {{dtensor.add: expected pairwise SSA-identical dims for lhs/rhs}}
  %bad = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%d0, %d0, %d0, %d0], f32>, !dtensor.tensor<[%d1, %d1, %d1, %d1], f32>) -> !dtensor.tensor<[%d0, %d0, %d0, %d0], f32>
}

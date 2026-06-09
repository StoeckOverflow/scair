// Purpose: Rank-0 policy + high-rank symbolic-shape coverage without duplicating existing mismatch suites.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefixes=CANON,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefixes=PIPE,DIAG

// Rank-0 tensors are valid: empty/fill/cast/add/mul.
builtin.module {
  %zero = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
  %e = "d_tensor.empty"() : () -> !d_tensor.tensor<[], f32>
  %f = "d_tensor.fill"(%zero) : (f32) -> !d_tensor.tensor<[], f32>
  %c = "d_tensor.cast"(%e) : (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
  %s = "d_tensor.add"(%e, %f)
    : (!d_tensor.tensor<[], f32>, !d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
  %p = "d_tensor.mul"(%s, %c)
    : (!d_tensor.tensor<[], f32>, !d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
  "test.keep"(%p) : (!d_tensor.tensor<[], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// VERIFY:   %1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[], f32>
// VERIFY:   %2 = "d_tensor.fill"(%0) : (f32) -> !d_tensor.tensor<[], f32>
// VERIFY:   %3 = "d_tensor.cast"(%1) : (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
// VERIFY:   %4 = "d_tensor.add"(%1, %2) : (!d_tensor.tensor<[], f32>, !d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
// VERIFY:   %5 = "d_tensor.mul"(%4, %3) : (!d_tensor.tensor<[], f32>, !d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
// VERIFY:   "test.keep"(%5) : (!d_tensor.tensor<[], f32>) -> ()
// VERIFY: }
// CANON: builtin.module {
// CANON:   %0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// CANON:   %1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[], f32>
// CANON:   %2 = "d_tensor.fill"(%0) : (f32) -> !d_tensor.tensor<[], f32>
// CANON:   %3 = "d_tensor.cast"(%1) : (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
// CANON:   %4 = "d_tensor.add"(%1, %2) : (!d_tensor.tensor<[], f32>, !d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
// CANON:   %5 = "d_tensor.mul"(%4, %3) : (!d_tensor.tensor<[], f32>, !d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
// CANON:   "test.keep"(%5) : (!d_tensor.tensor<[], f32>) -> ()
// CANON: }
// PIPE: builtin.module {
// PIPE:   %0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// PIPE:   %1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[], f32>
// PIPE:   %2 = "d_tensor.fill"(%0) : (f32) -> !d_tensor.tensor<[], f32>
// PIPE:   %3 = "d_tensor.cast"(%1) : (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
// PIPE:   %4 = "d_tensor.add"(%1, %2) : (!d_tensor.tensor<[], f32>, !d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
// PIPE:   %5 = "d_tensor.mul"(%4, %3) : (!d_tensor.tensor<[], f32>, !d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
// PIPE:   "test.keep"(%5) : (!d_tensor.tensor<[], f32>) -> ()
// PIPE: }

// -----

// Rank-0 dim query must fail (axis out of bounds).
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %e = "d_tensor.empty"() : () -> !d_tensor.tensor<[], f32>
  // expected-error @below {{d_tensor.dim: axis 0 out of bounds for rank 0}}
  %d = "d_tensor.dim"(%e) <{axis = 0 : i32}> : (!d_tensor.tensor<[], f32>) -> !value<%m>
}

// DIAG: d_tensor.dim: axis 0 out of bounds for rank 0

// -----

// Valid high-rank (rank 5) with repeated symbolic dims.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "d_tensor.empty"() : () -> !d_tensor.tensor<[%n, %n, %m, %n, %n], f32>
  %b = "d_tensor.empty"() : () -> !d_tensor.tensor<[%n, %n, %m, %n, %n], f32>
  %s = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%n, %n, %m, %n, %n], f32>, !d_tensor.tensor<[%n, %n, %m, %n, %n], f32>) -> !d_tensor.tensor<[%n, %n, %m, %n, %n], f32>
  %p = "d_tensor.mul"(%s, %a)
    : (!d_tensor.tensor<[%n, %n, %m, %n, %n], f32>, !d_tensor.tensor<[%n, %n, %m, %n, %n], f32>) -> !d_tensor.tensor<[%n, %n, %m, %n, %n], f32>
  "test.keep"(%p) : (!d_tensor.tensor<[%n, %n, %m, %n, %n], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %2 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%1, %1, %0, %1, %1], f32>
// VERIFY:   %3 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%1, %1, %0, %1, %1], f32>
// VERIFY:   %4 = "d_tensor.add"(%2, %3) : (!d_tensor.tensor<[%1, %1, %0, %1, %1], f32>, !d_tensor.tensor<[%1, %1, %0, %1, %1], f32>) -> !d_tensor.tensor<[%1, %1, %0, %1, %1], f32>
// VERIFY:   %5 = "d_tensor.mul"(%4, %2) : (!d_tensor.tensor<[%1, %1, %0, %1, %1], f32>, !d_tensor.tensor<[%1, %1, %0, %1, %1], f32>) -> !d_tensor.tensor<[%1, %1, %0, %1, %1], f32>
// VERIFY:   "test.keep"(%5) : (!d_tensor.tensor<[%1, %1, %0, %1, %1], f32>) -> ()
// VERIFY: }

// -----

// Invalid high-rank add: semantically equal dims but SSA-distinct params.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %z = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %d0 = "d_tensor.nat.add"(%m, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %d1 = "d_tensor.nat.add"(%m, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %a = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0, %d0, %d0, %d0], f32>
  %b = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d1, %d1, %d1, %d1], f32>
  // expected-error @below {{d_tensor.add: expected pairwise SSA-identical dims for lhs/rhs}}
  %bad = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%d0, %d0, %d0, %d0], f32>, !d_tensor.tensor<[%d1, %d1, %d1, %d1], f32>) -> !d_tensor.tensor<[%d0, %d0, %d0, %d0], f32>
}

// DIAG: d_tensor.add: expected pairwise SSA-identical dims for lhs/rhs

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: join the first two dimensions using an explicit nat.mul result.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %c = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%c) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %4 = "test.b"() : () -> !dtensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   %5 = "dtensor.join_dim"(%4) <{dim = 0 : i32}> : (!dtensor.tensor<[%0, %1, %2], f32>) -> !dtensor.tensor<[%3, %2], f32>
// CHECK-NEXT:   "test.keep"(%5) : (!dtensor.tensor<[%3, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: join the second and third dimensions.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.mul"(%nt, %tn) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%m, %nt, %tn], f32>
  %c = "dtensor.join_dim"(%b) <{dim = 1 : i32}>
    : (!dtensor.tensor<[%m, %nt, %tn], f32>) -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%c) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.mul"(%1, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %4 = "test.b"() : () -> !dtensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   %5 = "dtensor.join_dim"(%4) <{dim = 1 : i32}> : (!dtensor.tensor<[%0, %1, %2], f32>) -> !dtensor.tensor<[%0, %3], f32>
// CHECK-NEXT:   "test.keep"(%5) : (!dtensor.tensor<[%0, %3], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: join result dimension may be unrelated before canonicalization.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %other = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %structural = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%other], f32>
  "test.keep"(%structural) : (!dtensor.tensor<[%other], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.b"() : () -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "dtensor.join_dim"(%3) <{dim = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: missing product provenance is materialized by tensor-shape-canonicalize.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %structural = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%m], f32>
  "test.keep"(%structural) : (!dtensor.tensor<[%m], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.b"() : () -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "dtensor.join_dim"(%3) <{dim = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: product order is not checked by the verifier.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%tm, %mt) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %structural = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%m], f32>
  "test.keep"(%structural) : (!dtensor.tensor<[%m], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.mul"(%1, %0) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.b"() : () -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "dtensor.join_dim"(%3) <{dim = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: join dim out of bounds.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %bad = "dtensor.join_dim"(%b) <{dim = 1 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%m], f32>
}

// CHECK: dtensor.join_dim: dim 1 out of bounds for rank 2

// -----

// Invalid: join wrong result rank.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %bad = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%m, %mt], f32>
}

// CHECK: dtensor.join_dim: expected result rank = input rank - 1

// -----

// Invalid: element type mismatch is rejected for join.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %bad = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%m], i32>
}

// CHECK: dtensor.join_dim: expected equal element types

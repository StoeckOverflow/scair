// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: join the first two dimensions using an explicit size.mul result.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.mul"(%mt, %tm) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm, %n], f32>
  %c = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%c) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %3 = "d_tensor.size.mul"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CHECK-NEXT:   %4 = "test.b"() : () -> !d_tensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   %5 = "d_tensor.join_dim"(%4) <{dim = 0 : i32}> : (!d_tensor.tensor<[%0, %1, %2], f32>) -> !d_tensor.tensor<[%3, %2], f32>
// CHECK-NEXT:   "test.keep"(%5) : (!d_tensor.tensor<[%3, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: join the second and third dimensions.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %nt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tn = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.mul"(%nt, %tn) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %b = "test.b"() : () -> !d_tensor.tensor<[%m, %nt, %tn], f32>
  %c = "d_tensor.join_dim"(%b) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m, %nt, %tn], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%c) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %3 = "d_tensor.size.mul"(%1, %2) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CHECK-NEXT:   %4 = "test.b"() : () -> !d_tensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   %5 = "d_tensor.join_dim"(%4) <{dim = 1 : i32}> : (!d_tensor.tensor<[%0, %1, %2], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// CHECK-NEXT:   "test.keep"(%5) : (!d_tensor.tensor<[%0, %3], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: join result dimension may be unrelated before canonicalization.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %other = "d_tensor.size.param"() : () -> !d_tensor.size
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %structural = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%other], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%other], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %3 = "test.b"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.join_dim"(%3) <{dim = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: missing product provenance is materialized by tensor-shape-canonicalize.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %structural = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %3 = "test.b"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.join_dim"(%3) <{dim = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: product order is not checked by the verifier.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.mul"(%tm, %mt) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %structural = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.mul"(%1, %0) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CHECK-NEXT:   %3 = "test.b"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.join_dim"(%3) <{dim = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: join dim out of bounds.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.mul"(%mt, %tm) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %bad = "d_tensor.join_dim"(%b) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.join_dim: dim 1 out of bounds for rank 2

// -----

// Invalid: join wrong result rank.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.mul"(%mt, %tm) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %bad = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m, %mt], f32>
}

// CHECK: d_tensor.join_dim: expected result rank = input rank - 1

// -----

// Invalid: element type mismatch is rejected for join.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.mul"(%mt, %tm) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %bad = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], i32>
}

// CHECK: d_tensor.join_dim: expected equal element types

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

// Valid structural IR: no product/equality witness is present before canonicalization.
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

// Valid structural IR: no required product provenance.
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

// -----

// Valid: join a middle pair in a higher-rank tensor.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %nt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tn = "d_tensor.size.param"() : () -> !d_tensor.size
  %p = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %nt, %tn, %p], f32>
  %joined = "d_tensor.join_dim"(%src) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m, %nt, %tn, %p], f32>) -> !d_tensor.tensor<[%m, %n, %p], f32>
  "test.keep"(%joined) : (!d_tensor.tensor<[%m, %n, %p], f32>) -> ()
}

// CHECK: "d_tensor.join_dim"
// CHECK-SAME: (!d_tensor.tensor<[%0, %1, %2, %3], f32>) -> !d_tensor.tensor<[%0, %4, %3], f32>

// -----

// Valid: static size.const dimensions still use the SSA-dimension path.
builtin.module {
  %mt = "d_tensor.size.const"() <{value = 2 : i32}> : () -> !d_tensor.size
  %tm = "d_tensor.size.const"() <{value = 3 : i32}> : () -> !d_tensor.size
  %m = "d_tensor.size.const"() <{value = 6 : i32}> : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %joined = "d_tensor.join_dim"(%src) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
  "test.keep"(%joined) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// CHECK: "d_tensor.join_dim"
// CHECK-SAME: (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>

// -----

// Invalid: dim attribute must be i32.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %bad = "d_tensor.join_dim"(%src) <{dim = 0 : i64}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.join_dim: expected i32 dim attribute

// -----

// Invalid: dim must be non-negative.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %bad = "d_tensor.join_dim"(%src) <{dim = -1 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.join_dim: dim -1 out of bounds for rank 2

// -----

// Invalid: rank-0 tensors have no adjacent dimensions to join.
builtin.module {
  %src = "test.src"() : () -> !d_tensor.tensor<[], f32>
  %bad = "d_tensor.join_dim"(%src) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
}

// CHECK: d_tensor.join_dim: expected result rank = input rank - 1

// -----

// Invalid: dimensions before the joined axis must be preserved.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %other = "d_tensor.size.param"() : () -> !d_tensor.size
  %nt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tn = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %nt, %tn], f32>
  %bad = "d_tensor.join_dim"(%src) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m, %nt, %tn], f32>) -> !d_tensor.tensor<[%other, %n], f32>
}

// CHECK: d_tensor.join_dim: expected dimensions before joined dim to be SSA-identical

// -----

// Invalid: dimensions after the joined axis must be preserved.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %other = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%mt, %tm, %n], f32>
  %bad = "d_tensor.join_dim"(%src) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm, %n], f32>) -> !d_tensor.tensor<[%m, %other], f32>
}

// CHECK: d_tensor.join_dim: expected dimensions after joined dim to be shifted and SSA-identical

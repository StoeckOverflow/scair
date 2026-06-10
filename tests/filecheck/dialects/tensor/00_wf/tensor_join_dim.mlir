// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: join the first two dimensions using an explicit arith.muli result.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %m = "arith.muli"(%mt, %tm) : (index, index) -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm, %n], f32>
  %c = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%c) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "test.index"() : () -> index
// CHECK-NEXT:   %1 = "test.index"() : () -> index
// CHECK-NEXT:   %2 = "test.index"() : () -> index
// CHECK-NEXT:   %3 = "arith.muli"(%0, %1) {{.*}} : (index, index) -> index
// CHECK-NEXT:   %4 = "test.b"() : () -> !d_tensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   %5 = "d_tensor.join_dim"(%4) <{dim = 0 : i32}> : (!d_tensor.tensor<[%0, %1, %2], f32>) -> !d_tensor.tensor<[%3, %2], f32>
// CHECK-NEXT:   "test.keep"(%5) : (!d_tensor.tensor<[%3, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: join the second and third dimensions.
builtin.module {
  %m = "test.index"() : () -> index
  %nt = "test.index"() : () -> index
  %tn = "test.index"() : () -> index
  %n = "arith.muli"(%nt, %tn) : (index, index) -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%m, %nt, %tn], f32>
  %c = "d_tensor.join_dim"(%b) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m, %nt, %tn], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%c) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "test.index"() : () -> index
// CHECK-NEXT:   %1 = "test.index"() : () -> index
// CHECK-NEXT:   %2 = "test.index"() : () -> index
// CHECK-NEXT:   %3 = "arith.muli"(%1, %2) {{.*}} : (index, index) -> index
// CHECK-NEXT:   %4 = "test.b"() : () -> !d_tensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   %5 = "d_tensor.join_dim"(%4) <{dim = 1 : i32}> : (!d_tensor.tensor<[%0, %1, %2], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// CHECK-NEXT:   "test.keep"(%5) : (!d_tensor.tensor<[%0, %3], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: no product/equality witness is present before canonicalization.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %other = "test.index"() : () -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %structural = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%other], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%other], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "test.index"() : () -> index
// CHECK-NEXT:   %1 = "test.index"() : () -> index
// CHECK-NEXT:   %2 = "test.index"() : () -> index
// CHECK-NEXT:   %3 = "test.b"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.join_dim"(%3) <{dim = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: missing product provenance is materialized by tensor-shape-canonicalize.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %m = "test.index"() : () -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %structural = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "test.index"() : () -> index
// CHECK-NEXT:   %1 = "test.index"() : () -> index
// CHECK-NEXT:   %2 = "test.index"() : () -> index
// CHECK-NEXT:   %3 = "test.b"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.join_dim"(%3) <{dim = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: no required product provenance.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %m = "test.index"() : () -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %structural = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "test.index"() : () -> index
// CHECK-NEXT:   %1 = "test.index"() : () -> index
// CHECK-NEXT:   %2 = "test.index"() : () -> index
// CHECK-NEXT:   %3 = "test.b"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.join_dim"(%3) <{dim = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: join dim out of bounds.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %m = "arith.muli"(%mt, %tm) : (index, index) -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %bad = "d_tensor.join_dim"(%b) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.join_dim: dim 1 out of bounds for rank 2

// -----

// Invalid: join wrong result rank.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %m = "arith.muli"(%mt, %tm) : (index, index) -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %bad = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m, %mt], f32>
}

// CHECK: d_tensor.join_dim: expected result rank = input rank - 1

// -----

// Invalid: element type mismatch is rejected for join.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %m = "arith.muli"(%mt, %tm) : (index, index) -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %bad = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], i32>
}

// CHECK: d_tensor.join_dim: expected equal element types

// -----

// Valid: join a middle pair in a higher-rank tensor.
builtin.module {
  %m = "test.index"() : () -> index
  %nt = "test.index"() : () -> index
  %tn = "test.index"() : () -> index
  %p = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %nt, %tn, %p], f32>
  %joined = "d_tensor.join_dim"(%src) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m, %nt, %tn, %p], f32>) -> !d_tensor.tensor<[%m, %n, %p], f32>
  "test.keep"(%joined) : (!d_tensor.tensor<[%m, %n, %p], f32>) -> ()
}

// CHECK: "d_tensor.join_dim"
// CHECK-SAME: (!d_tensor.tensor<[%0, %1, %2, %3], f32>) -> !d_tensor.tensor<[%0, %4, %3], f32>

// -----

// Valid: static arith.constant dimensions still use the SSA-dimension path.
builtin.module {
  %mt = "arith.constant"() <{value = 2 : index}> : () -> index
  %tm = "arith.constant"() <{value = 3 : index}> : () -> index
  %m = "arith.constant"() <{value = 6 : index}> : () -> index
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
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %m = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%mt, %tm], f32>
  %bad = "d_tensor.join_dim"(%src) <{dim = 0 : i64}>
    : (!d_tensor.tensor<[%mt, %tm], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.join_dim: expected i32 dim attribute

// -----

// Invalid: dim must be non-negative.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %m = "test.index"() : () -> index
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
  %m = "test.index"() : () -> index
  %other = "test.index"() : () -> index
  %nt = "test.index"() : () -> index
  %tn = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %nt, %tn], f32>
  %bad = "d_tensor.join_dim"(%src) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m, %nt, %tn], f32>) -> !d_tensor.tensor<[%other, %n], f32>
}

// CHECK: d_tensor.join_dim: expected dimensions before joined dim to be SSA-identical

// -----

// Invalid: dimensions after the joined axis must be preserved.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %other = "test.index"() : () -> index
  %m = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%mt, %tm, %n], f32>
  %bad = "d_tensor.join_dim"(%src) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm, %n], f32>) -> !d_tensor.tensor<[%m, %other], f32>
}

// CHECK: d_tensor.join_dim: expected dimensions after joined dim to be shifted and SSA-identical

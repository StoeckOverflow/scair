// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: split one dimension with explicit output dimensions.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %b = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm], f32>
  "test.keep"(%b) : (!d_tensor.tensor<[%mt, %tm], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "test.index"() : () -> index
// CHECK-NEXT:   %1 = "test.index"() : () -> index
// CHECK-NEXT:   %2 = "test.index"() : () -> index
// CHECK-NEXT:   %3 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// CHECK-NEXT:   %4 = "d_tensor.split_dim"(%3, %1, %2) <{dim = 0 : i32}> : (!d_tensor.tensor<[%0], f32>, index, index) -> !d_tensor.tensor<[%1, %2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%1, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: split the first dimension of a 2D tensor.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %b = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm, %n], f32>
  "test.keep"(%b) : (!d_tensor.tensor<[%mt, %tm, %n], f32>) -> ()
}

// CHECK: "d_tensor.split_dim"
// CHECK-SAME: (!d_tensor.tensor<[%0, %3], f32>, index, index) -> !d_tensor.tensor<[%1, %2, %3], f32>

// -----

// Valid: split the second dimension of a 2D tensor.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %nt = "test.index"() : () -> index
  %tn = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %b = "d_tensor.split_dim"(%a, %nt, %tn) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>, index, index)
      -> !d_tensor.tensor<[%m, %nt, %tn], f32>
  "test.keep"(%b) : (!d_tensor.tensor<[%m, %nt, %tn], f32>) -> ()
}

// CHECK: "d_tensor.split_dim"
// CHECK-SAME: (!d_tensor.tensor<[%0, %1], f32>, index, index) -> !d_tensor.tensor<[%0, %2, %3], f32>

// -----

// Valid: split the middle dimension of a higher-rank tensor.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %p = "test.index"() : () -> index
  %nt = "test.index"() : () -> index
  %tn = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n, %p], f32>
  %b = "d_tensor.split_dim"(%a, %nt, %tn) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m, %n, %p], f32>, index, index)
      -> !d_tensor.tensor<[%m, %nt, %tn, %p], f32>
  "test.keep"(%b) : (!d_tensor.tensor<[%m, %nt, %tn, %p], f32>) -> ()
}

// CHECK: "d_tensor.split_dim"
// CHECK-SAME: (!d_tensor.tensor<[%0, %1, %2], f32>, index, index) -> !d_tensor.tensor<[%0, %3, %4, %2], f32>

// -----

// Valid: static arith.constant dimensions still use the SSA-dimension path.
builtin.module {
  %m = "arith.constant"() <{value = 6 : index}> : () -> index
  %mt = "arith.constant"() <{value = 2 : index}> : () -> index
  %tm = "arith.constant"() <{value = 3 : index}> : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %b = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm], f32>
  "test.keep"(%b) : (!d_tensor.tensor<[%mt, %tm], f32>) -> ()
}

// CHECK: "d_tensor.split_dim"
// CHECK-SAME: (!d_tensor.tensor<[%0], f32>, index, index) -> !d_tensor.tensor<[%1, %2], f32>

// -----

// Invalid: outer operand must match the first inserted result dimension.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %other = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%other, %tm], f32>
}

// CHECK: d_tensor.split_dim: outer operand must be SSA-identical to result dimension 0

// -----

// Invalid: inner operand must match the second inserted result dimension.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %other = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %other], f32>
}

// CHECK: d_tensor.split_dim: inner operand must be SSA-identical to result dimension 1

// -----

// Invalid: dimensions before the split axis must be preserved.
builtin.module {
  %m = "test.index"() : () -> index
  %other = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %nt = "test.index"() : () -> index
  %tn = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.split_dim"(%a, %nt, %tn) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>, index, index)
      -> !d_tensor.tensor<[%other, %nt, %tn], f32>
}

// CHECK: d_tensor.split_dim: expected dimensions before split dim to be SSA-identical

// -----

// Invalid: dimensions after the split axis must be preserved.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %other = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm, %other], f32>
}

// CHECK: d_tensor.split_dim: expected dimensions after split dim to be shifted and SSA-identical

// -----

// Invalid: split dim out of bounds.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 1 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm], f32>
}

// CHECK: d_tensor.split_dim: dim 1 out of bounds for rank 1

// -----

// Invalid: result rank must be input rank plus one.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm, %m], f32>
}

// CHECK: d_tensor.split_dim: expected result rank = input rank + 1

// -----

// Invalid: element type mismatch is rejected.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm], i32>
}

// CHECK: d_tensor.split_dim: expected equal element types

// -----

// Invalid: dim attribute must be i32.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = 0 : i64}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm], f32>
}

// CHECK: d_tensor.split_dim: expected i32 dim attribute

// -----

// Invalid: dim must be non-negative.
builtin.module {
  %m = "test.index"() : () -> index
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.split_dim"(%a, %mt, %tm) <{dim = -1 : i32}>
    : (!d_tensor.tensor<[%m], f32>, index, index)
      -> !d_tensor.tensor<[%mt, %tm], f32>
}

// CHECK: d_tensor.split_dim: dim -1 out of bounds for rank 1

// -----

// Invalid: rank-0 tensors have no dimension to split.
builtin.module {
  %outer = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[], f32>
  %bad = "d_tensor.split_dim"(%src, %outer, %outer) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[], f32>, index, index)
      -> !d_tensor.tensor<[%outer], f32>
}

// CHECK: d_tensor.split_dim: dim 0 out of bounds for rank 0

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: expand_shape carries explicit output dimensions like baseline tensor.expand_shape.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %mn = "test.index"() : () -> index
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %expanded = "d_tensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%expanded) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "test.index"() : () -> index
// CHECK-NEXT:   %1 = "test.index"() : () -> index
// CHECK-NEXT:   %2 = "test.index"() : () -> index
// CHECK-NEXT:   %3 = "test.flat"() : () -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   %4 = "d_tensor.expand_shape"(%3, %0, %1) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%2], f32>, index, index) -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%0, %1], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: rank-preserving expand_shape is just structural.
builtin.module {
  %m = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %same = "d_tensor.expand_shape"(%src, %m)
    <{reassociation = [[0 : i32]]}>
    : (!d_tensor.tensor<[%m], f32>, index) -> !d_tensor.tensor<[%m], f32>
  "test.keep"(%same) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// CHECK: "d_tensor.expand_shape"
// CHECK-SAME: (!d_tensor.tensor<[%0], f32>, index) -> !d_tensor.tensor<[%0], f32>

// -----

// Valid: rank-0 expand_shape identity.
builtin.module {
  %src = "test.src"() : () -> !d_tensor.tensor<[], f32>
  %same = "d_tensor.expand_shape"(%src)
    <{reassociation = []}>
    : (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
  "test.keep"(%same) : (!d_tensor.tensor<[], f32>) -> ()
}

// CHECK: "d_tensor.expand_shape"
// CHECK-SAME: <{reassociation = {{\[\]}}}>
// CHECK-SAME: (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>

// -----

// Valid: rank-preserving expand_shape identity across multiple dims.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %same = "d_tensor.expand_shape"(%src, %m, %n)
    <{reassociation = [[0 : i32], [1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%same) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: "d_tensor.expand_shape"
// CHECK-SAME: reassociation = {{\[\[0 : i32\], \[1 : i32\]\]}}

// -----

// Valid: higher-rank expansion with two reassociation groups.
builtin.module {
  %a = "test.index"() : () -> index
  %b = "test.index"() : () -> index
  %c = "test.index"() : () -> index
  %d = "test.index"() : () -> index
  %ab = "test.index"() : () -> index
  %cd = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%ab, %cd], f32>
  %expanded = "d_tensor.expand_shape"(%src, %a, %b, %c, %d)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32]]}>
    : (!d_tensor.tensor<[%ab, %cd], f32>, index, index, index, index)
      -> !d_tensor.tensor<[%a, %b, %c, %d], f32>
  "test.keep"(%expanded) : (!d_tensor.tensor<[%a, %b, %c, %d], f32>) -> ()
}

// CHECK: "d_tensor.expand_shape"
// CHECK-SAME: reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}

// -----

// Valid: static arith.constant dimensions still use the SSA-dimension path.
builtin.module {
  %m = "arith.constant"() <{value = 2 : index}> : () -> index
  %n = "arith.constant"() <{value = 3 : index}> : () -> index
  %flat = "arith.constant"() <{value = 6 : index}> : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%flat], f32>
  %expanded = "d_tensor.expand_shape"(%src, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%flat], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%expanded) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: "d_tensor.expand_shape"
// CHECK-SAME: (!d_tensor.tensor<[%2], f32>, index, index) -> !d_tensor.tensor<[%0, %1], f32>

// -----

// Invalid: output shape operands must match the result type dimensions.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %other = "test.index"() : () -> index
  %mn = "test.index"() : () -> index
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, index, index)
      -> !d_tensor.tensor<[%m, %other], f32>
}

// CHECK: d_tensor.expand_shape: output shape operand 1 must be SSA-identical to result dimension 1

// -----

// Invalid: output shape operand count must equal result rank.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %mn = "test.index"() : () -> index
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%flat, %m)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, index) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.expand_shape: expected 2 output shape operands, got 1

// -----

// Invalid: reassociation must cover result dims contiguously.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %mn = "test.index"() : () -> index
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[1 : i32, 0 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.expand_shape: reassociation must cover result dims contiguously

// -----

// Invalid: element type mismatch is rejected.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %mn = "test.index"() : () -> index
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], i32>
}

// CHECK: d_tensor.expand_shape: expected equal element types

// -----

// Invalid: expand_shape cannot reduce rank.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %out = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.expand_shape"(%src, %out)
    <{reassociation = [[0 : i32], [1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>, index)
      -> !d_tensor.tensor<[%out], f32>
}

// CHECK: d_tensor.expand_shape: expected result rank >= source rank

// -----

// Invalid: reassociation group must be an array attribute.
builtin.module {
  %m = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.expand_shape"(%src, %m)
    <{reassociation = [0 : i32]}>
    : (!d_tensor.tensor<[%m], f32>, index) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.expand_shape: reassociation group 0 must be an array attribute

// -----

// Invalid: reassociation indices must be i32 integer attributes.
builtin.module {
  %m = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.expand_shape"(%src, %m)
    <{reassociation = [[0 : i64]]}>
    : (!d_tensor.tensor<[%m], f32>, index) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.expand_shape: reassociation group 0 index 0 must be an i32 integer attribute

// -----

// Invalid: reassociation group count must equal source rank.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.expand_shape"(%src, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.expand_shape: expected 2 reassociation groups, got 1

// -----

// Invalid: reassociation groups must be non-empty.
builtin.module {
  %m = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.expand_shape"(%src, %m)
    <{reassociation = [[]]}>
    : (!d_tensor.tensor<[%m], f32>, index) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.expand_shape: reassociation groups must be non-empty

// -----

// Invalid: reassociation indices must be non-negative.
builtin.module {
  %m = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.expand_shape"(%src, %m)
    <{reassociation = [[-1 : i32]]}>
    : (!d_tensor.tensor<[%m], f32>, index) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.expand_shape: reassociation indices must be non-negative

// -----

// Invalid: reassociation must not miss a result dim.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %mn = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%src, %m, %n)
    <{reassociation = [[0 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.expand_shape: reassociation must cover result dims contiguously

// -----

// Invalid: reassociation must not duplicate a result dim.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %mn = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%src, %m, %n)
    <{reassociation = [[0 : i32, 0 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.expand_shape: reassociation must cover result dims contiguously

// -----

// Invalid: reassociation must not name an extra result dim.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %mn = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%src, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32, 2 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, index, index)
      -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.expand_shape: reassociation must cover result dims contiguously

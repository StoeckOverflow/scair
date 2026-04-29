// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: one direct nat.mul-backed dimension split.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %q4 = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
  "test.keep"(%q4) : (!dtensor.tensor<[%b, %s, %heads, %head_dim], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %4 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %5 = "test.q"() : () -> !dtensor.tensor<[%0, %1, %4], f32>
// CHECK-NEXT:   %6 = "dtensor.expand_shape"(%5) <{reassociation = {{\[\[0 : i32\], \[1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!dtensor.tensor<[%0, %1, %4], f32>) -> !dtensor.tensor<[%0, %1, %2, %3], f32>
// CHECK-NEXT:   "test.keep"(%6) : (!dtensor.tensor<[%0, %1, %2, %3], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: result rank must be source rank + 1 for v1.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %hidden], f32>
}

// CHECK: dtensor.expand_shape: v1 expected result rank = source rank + 1

// -----

// Invalid: element type must be preserved.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], i32>
}

// CHECK: dtensor.expand_shape: expected equal element types

// -----

// Invalid: split source dim must be direct nat.mul provenance.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: split source dim must be produced by direct dtensor.nat.mul

// -----

// Invalid: nat.mul operands must match result dims in reassociation order.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %head_dim, %heads], f32>
}

// CHECK: dtensor.expand_shape: split lhs must match the first result split dim

// -----

// Invalid: rhs factor must match the second result split dim.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %other = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %other], f32>
}

// CHECK: dtensor.expand_shape: split rhs must match the second result split dim

// -----

// Invalid: unchanged dims must be SSA-identical.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %b_other = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b_other, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: expected unchanged dim 0 to be SSA-identical to result dim 0

// -----

// Invalid: reassociation must be contiguous and complete.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [2 : i32], [1 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation must cover result dims contiguously

// -----

// Invalid: two split groups are outside v1 and are rejected by the rank guard.
builtin.module {
  %b0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %b1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %s0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %s1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "dtensor.nat.mul"(%b0, %b1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %hidden], f32>) -> !dtensor.tensor<[%b0, %b1, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: v1 expected result rank = source rank + 1

// -----

// Invalid: reassociation group must be an array attribute.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [0 : i32, [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation group 0 must be an array attribute

// -----

// Invalid: reassociation indices must be i32 integer attributes.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i64], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation group 0 index 0 must be an i32 integer attribute

// -----

// Invalid: reassociation groups must be non-empty.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [], [1 : i32, 2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation groups must be non-empty

// -----

// Invalid: reassociation indices must be non-negative.
builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %bad = "dtensor.expand_shape"(%q)
    <{reassociation = [[-1 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
}

// CHECK: dtensor.expand_shape: reassociation indices must be non-negative

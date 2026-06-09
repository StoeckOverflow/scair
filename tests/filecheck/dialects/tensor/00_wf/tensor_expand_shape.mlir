// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: expand_shape carries explicit output dimensions like baseline tensor.expand_shape.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %expanded = "d_tensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, !d_tensor.nat, !d_tensor.nat)
      -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%expanded) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %3 = "test.flat"() : () -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   %4 = "d_tensor.expand_shape"(%3, %0, %1) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%2], f32>, !d_tensor.nat, !d_tensor.nat) -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%0, %1], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: rank-preserving expand_shape is just structural.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %same = "d_tensor.expand_shape"(%src, %m)
    <{reassociation = [[0 : i32]]}>
    : (!d_tensor.tensor<[%m], f32>, !d_tensor.nat) -> !d_tensor.tensor<[%m], f32>
  "test.keep"(%same) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// CHECK: "d_tensor.expand_shape"
// CHECK-SAME: (!d_tensor.tensor<[%0], f32>, !d_tensor.nat) -> !d_tensor.tensor<[%0], f32>

// -----

// Invalid: output shape operands must match the result type dimensions.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %other = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, !d_tensor.nat, !d_tensor.nat)
      -> !d_tensor.tensor<[%m, %other], f32>
}

// CHECK: d_tensor.expand_shape: output shape operand 1 must be SSA-identical to result dimension 1

// -----

// Invalid: output shape operand count must equal result rank.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%flat, %m)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, !d_tensor.nat) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.expand_shape: expected 2 output shape operands, got 1

// -----

// Invalid: reassociation must cover result dims contiguously.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[1 : i32, 0 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, !d_tensor.nat, !d_tensor.nat)
      -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.expand_shape: reassociation must cover result dims contiguously

// -----

// Invalid: element type mismatch is rejected.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %flat = "test.flat"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>, !d_tensor.nat, !d_tensor.nat)
      -> !d_tensor.tensor<[%m, %n], i32>
}

// CHECK: d_tensor.expand_shape: expected equal element types

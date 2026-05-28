// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: expand_shape carries explicit output dimensions like baseline tensor.expand_shape.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.param"() : () -> !dtensor.nat
  %flat = "test.flat"() : () -> !dtensor.tensor<[%mn], f32>
  %expanded = "dtensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%mn], f32>, !dtensor.nat, !dtensor.nat)
      -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%expanded) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.flat"() : () -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   %4 = "dtensor.expand_shape"(%3, %0, %1) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%2], f32>, !dtensor.nat, !dtensor.nat) -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%0, %1], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: rank-preserving expand_shape is just structural.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %src = "test.src"() : () -> !dtensor.tensor<[%m], f32>
  %same = "dtensor.expand_shape"(%src, %m)
    <{reassociation = [[0 : i32]]}>
    : (!dtensor.tensor<[%m], f32>, !dtensor.nat) -> !dtensor.tensor<[%m], f32>
  "test.keep"(%same) : (!dtensor.tensor<[%m], f32>) -> ()
}

// CHECK: "dtensor.expand_shape"
// CHECK-SAME: (!dtensor.tensor<[%0], f32>, !dtensor.nat) -> !dtensor.tensor<[%0], f32>

// -----

// Invalid: output shape operands must match the result type dimensions.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %other = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.param"() : () -> !dtensor.nat
  %flat = "test.flat"() : () -> !dtensor.tensor<[%mn], f32>
  %bad = "dtensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%mn], f32>, !dtensor.nat, !dtensor.nat)
      -> !dtensor.tensor<[%m, %other], f32>
}

// CHECK: dtensor.expand_shape: output shape operand 1 must be SSA-identical to result dimension 1

// -----

// Invalid: output shape operand count must equal result rank.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.param"() : () -> !dtensor.nat
  %flat = "test.flat"() : () -> !dtensor.tensor<[%mn], f32>
  %bad = "dtensor.expand_shape"(%flat, %m)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%mn], f32>, !dtensor.nat) -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK: dtensor.expand_shape: expected 2 output shape operands, got 1

// -----

// Invalid: reassociation must cover result dims contiguously.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.param"() : () -> !dtensor.nat
  %flat = "test.flat"() : () -> !dtensor.tensor<[%mn], f32>
  %bad = "dtensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[1 : i32, 0 : i32]]}>
    : (!dtensor.tensor<[%mn], f32>, !dtensor.nat, !dtensor.nat)
      -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK: dtensor.expand_shape: reassociation must cover result dims contiguously

// -----

// Invalid: element type mismatch is rejected.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.param"() : () -> !dtensor.nat
  %flat = "test.flat"() : () -> !dtensor.tensor<[%mn], f32>
  %bad = "dtensor.expand_shape"(%flat, %m, %n)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%mn], f32>, !dtensor.nat, !dtensor.nat)
      -> !dtensor.tensor<[%m, %n], i32>
}

// CHECK: dtensor.expand_shape: expected equal element types

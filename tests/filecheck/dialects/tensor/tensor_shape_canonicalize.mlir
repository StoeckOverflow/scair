// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize | filecheck %s -DFILE=%s

// add(x, 0) -> x with deep RAUW into type-embedded dims.
builtin.module {
  %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %z = "tensor.nat.const"() <{value = 0 : i32}> : () -> !tensor.nat
  %s = "tensor.nat.add"(%m, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %u = "test.use"() : () -> !tensor.tensor<[%s], f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: [[M:%[0-9]+]] = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
// CHECK-NOT: "tensor.nat.add"
// CHECK: "test.use"() : () -> !tensor.tensor<[[[M]]], f32>
// CHECK: }

// -----

// mul(x, 1) -> x and mul(x, 0) -> 0.
builtin.module {
  %x = "tensor.nat.const"() <{value = 7 : i32}> : () -> !tensor.nat
  %one = "tensor.nat.const"() <{value = 1 : i32}> : () -> !tensor.nat
  %zero = "tensor.nat.const"() <{value = 0 : i32}> : () -> !tensor.nat
  %m1 = "tensor.nat.mul"(%x, %one) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %m0 = "tensor.nat.mul"(%m1, %zero) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %u = "test.use"() : () -> !tensor.tensor<[%m0], f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: [[ZERO:%[0-9]+]] = "tensor.nat.const"() <{value = 0 : i32}> : () -> !tensor.nat
// CHECK-NOT: "tensor.nat.mul"
// CHECK: "test.use"() : () -> !tensor.tensor<[[[ZERO]]], f32>
// CHECK: }

// -----

// Constant-fold nat.add/nat.mul.
builtin.module {
  %a = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %b = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %c = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %s = "tensor.nat.add"(%a, %b) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %p = "tensor.nat.mul"(%s, %c) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %u = "test.use"() : () -> !tensor.tensor<[%p], f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: "tensor.nat.const"() <{value = 20 : i32}> : () -> !tensor.nat
// CHECK-NOT: "tensor.nat.add"
// CHECK-NOT: "tensor.nat.mul"
// CHECK: "test.use"()
// CHECK: }

// -----

// dim fold to exact embedded dim SSA value.
builtin.module {
  %m = "tensor.nat.const"() <{value = 6 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 9 : i32}> : () -> !tensor.nat
  %A = "test.A"() : () -> !tensor.tensor<[%m, %n], f32>
  %d0 = "tensor.dim"(%A) <{axis = 0 : i32}>
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.nat
  %E = "tensor.empty"() : () -> !tensor.tensor<[%d0], f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: [[M:%[0-9]+]] = "tensor.nat.const"() <{value = 6 : i32}> : () -> !tensor.nat
// CHECK-NOT: "tensor.dim"
// CHECK: "tensor.empty"() : () -> !tensor.tensor<[[[M]]], f32>
// CHECK: }

// -----

// Must-not-fold: no neutral/constant identities present.
builtin.module {
  %x = "test.nat"() : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 7 : i32}> : () -> !tensor.nat
  %s = "tensor.nat.add"(%x, %n) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %u = "test.use"() : () -> !tensor.tensor<[%s], f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: "tensor.nat.add"(%0, %1)
// CHECK: "test.use"() : () -> !tensor.tensor<[%2], f32>
// CHECK: }

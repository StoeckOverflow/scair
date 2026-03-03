// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize | filecheck %s -DFILE=%s

// add(x, 0) -> x with deep RAUW into type-embedded dims.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.use"() : () -> !dtensor.tensor<[%s], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// CHECK:   %1 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// CHECK:   %2 = "test.use"() : () -> !dtensor.tensor<[%0], f32>
// CHECK: }

// -----

// mul(x, 1) -> x and mul(x, 0) -> 0.
builtin.module {
  %x = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
  %one = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  %zero = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %m1 = "dtensor.nat.mul"(%x, %one) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %m0 = "dtensor.nat.mul"(%m1, %zero) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.use"() : () -> !dtensor.tensor<[%m0], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
// CHECK:   %1 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// CHECK:   %2 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// CHECK:   %3 = "test.use"() : () -> !dtensor.tensor<[%2], f32>
// CHECK: }

// -----

// Constant-fold nat.add/nat.mul.
builtin.module {
  %a = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %b = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %c = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%a, %b) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %p = "dtensor.nat.mul"(%s, %c) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.use"() : () -> !dtensor.tensor<[%p], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
// CHECK:   %1 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
// CHECK:   %2 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// CHECK:   %3 = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
// CHECK:   %4 = "dtensor.nat.const"() <{value = 20 : i32}> : () -> !dtensor.nat
// CHECK:   %5 = "test.use"() : () -> !dtensor.tensor<[%4], f32>
// CHECK: }

// -----

// dtensor.dim remains (no dim-fold in strict !value<...> typing mode).
builtin.module {
  %m = "dtensor.nat.const"() <{value = 6 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 9 : i32}> : () -> !dtensor.nat
  %A = "test.A"() : () -> !dtensor.tensor<[%m, %n], f32>
  %d0 = "dtensor.dim"(%A) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "dtensor.empty"() : () -> !dtensor.tensor<[%d0], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "dtensor.nat.const"() <{value = 6 : i32}> : () -> !dtensor.nat
// CHECK:   %1 = "dtensor.nat.const"() <{value = 9 : i32}> : () -> !dtensor.nat
// CHECK:   %2 = "test.A"() : () -> !dtensor.tensor<[%0, %1], f32>
// CHECK:   %3 = "dtensor.dim"(%2) <{axis = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%0>
// CHECK:   %4 = "dtensor.empty"() : () -> !dtensor.tensor<[%3], f32>
// CHECK: }

// -----

// Must-not-fold: no neutral/constant identities present.
builtin.module {
  %x = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%x, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.use"() : () -> !dtensor.tensor<[%s], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK:   %1 = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
// CHECK:   %2 = "dtensor.nat.add"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK:   %3 = "test.use"() : () -> !dtensor.tensor<[%2], f32>
// CHECK: }

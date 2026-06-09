// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize | filecheck %s -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics

// add(x, 0) -> x with deep RAUW into type-embedded dims.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %z = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %s = "d_tensor.nat.add"(%m, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// CHECK:   %1 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// CHECK:   %2 = "test.use"() : () -> !d_tensor.tensor<[%0], f32>
// CHECK: }

// -----

// mul(x, 1) -> x and mul(x, 0) -> 0.
builtin.module {
  %x = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
  %one = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
  %zero = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %m1 = "d_tensor.nat.mul"(%x, %one) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %m0 = "d_tensor.nat.mul"(%m1, %zero) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %u = "test.use"() : () -> !d_tensor.tensor<[%m0], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
// CHECK:   %1 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// CHECK:   %2 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// CHECK:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CHECK: }

// -----

// Constant-fold nat.add/nat.mul.
builtin.module {
  %a = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %b = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %c = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %s = "d_tensor.nat.add"(%a, %b) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %p = "d_tensor.nat.mul"(%s, %c) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %u = "test.use"() : () -> !d_tensor.tensor<[%p], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
// CHECK:   %1 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
// CHECK:   %2 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// CHECK:   %3 = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
// CHECK:   %4 = "d_tensor.nat.const"() <{value = 20 : i32}> : () -> !d_tensor.nat
// CHECK:   %5 = "test.use"() : () -> !d_tensor.tensor<[%4], f32>
// CHECK: }

// -----

// d_tensor.dim remains (no dim-fold in strict !value<...> typing mode).
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 6 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 9 : i32}> : () -> !d_tensor.nat
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.nat.const"() <{value = 6 : i32}> : () -> !d_tensor.nat
// CHECK:   %1 = "d_tensor.nat.const"() <{value = 9 : i32}> : () -> !d_tensor.nat
// CHECK:   %2 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK:   %3 = "d_tensor.dim"(%2) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// CHECK:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%3], f32>
// CHECK: }

// -----

// Must-not-fold: no neutral/constant identities present.
builtin.module {
  %x = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
  %s = "d_tensor.nat.add"(%x, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK:   %1 = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
// CHECK:   %2 = "d_tensor.nat.add"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CHECK:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CHECK: }

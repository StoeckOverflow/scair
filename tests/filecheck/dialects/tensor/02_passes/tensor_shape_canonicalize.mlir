// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize | filecheck %s -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics

// add(x, 0) -> x with deep RAUW into type-embedded dims.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %z = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %s = "d_tensor.size.add"(%m, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// CHECK:   %1 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
// CHECK:   %2 = "test.use"() : () -> !d_tensor.tensor<[%0], f32>
// CHECK: }

// -----

// mul(x, 1) -> x and mul(x, 0) -> 0.
builtin.module {
  %x = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
  %one = "d_tensor.size.constant"() <{value = 1 : i32}> : () -> !d_tensor.size
  %zero = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %m1 = "d_tensor.size.mul"(%x, %one) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %m0 = "d_tensor.size.mul"(%m1, %zero) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u = "test.use"() : () -> !d_tensor.tensor<[%m0], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
// CHECK:   %1 = "d_tensor.size.constant"() <{value = 1 : i32}> : () -> !d_tensor.size
// CHECK:   %2 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
// CHECK:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CHECK: }

// -----

// Constant-fold size.add/size.mul.
builtin.module {
  %a = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %b = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %c = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %s = "d_tensor.size.add"(%a, %b) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %p = "d_tensor.size.mul"(%s, %c) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u = "test.use"() : () -> !d_tensor.tensor<[%p], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
// CHECK:   %1 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// CHECK:   %2 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// CHECK:   %3 = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
// CHECK:   %4 = "d_tensor.size.constant"() <{value = 20 : i32}> : () -> !d_tensor.size
// CHECK:   %5 = "test.use"() : () -> !d_tensor.tensor<[%4], f32>
// CHECK: }

// -----

// d_tensor.dim remains (no dim-fold in strict !value<...> typing mode).
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 6 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 9 : i32}> : () -> !d_tensor.size
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.size.constant"() <{value = 6 : i32}> : () -> !d_tensor.size
// CHECK:   %1 = "d_tensor.size.constant"() <{value = 9 : i32}> : () -> !d_tensor.size
// CHECK:   %2 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK:   %3 = "d_tensor.dim"(%2) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// CHECK:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%3], f32>
// CHECK: }

// -----

// Must-not-fold: no neutral/constant identities present.
builtin.module {
  %x = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
  %s = "d_tensor.size.add"(%x, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK:   %1 = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
// CHECK:   %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CHECK:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CHECK: }

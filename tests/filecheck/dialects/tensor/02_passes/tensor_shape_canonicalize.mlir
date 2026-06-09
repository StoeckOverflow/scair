// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize | filecheck %s -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics

// Index arith dimensions remain valid before the arith canonicalizer retargeting phase.
builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %s = "arith.addi"(%m, %z) : (index, index) -> index
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK:   %1 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CHECK:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CHECK: }

// -----

// Index multiplication dimensions are preserved by tensor-shape-canonicalize in Phase 1.
builtin.module {
  %x = "arith.constant"() <{value = 7 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %zero = "arith.constant"() <{value = 0 : index}> : () -> index
  %m1 = "arith.muli"(%x, %one) : (index, index) -> index
  %m0 = "arith.muli"(%m1, %zero) : (index, index) -> index
  %u = "test.use"() : () -> !d_tensor.tensor<[%m0], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "arith.constant"() <{value = 7 : index}> : () -> index
// CHECK:   %1 = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK:   %2 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK:   %3 = "arith.muli"(%0, %1) {{.*}} : (index, index) -> index
// CHECK:   %4 = "arith.muli"(%3, %2) {{.*}} : (index, index) -> index
// CHECK:   %5 = "test.use"() : () -> !d_tensor.tensor<[%4], f32>
// CHECK: }

// -----

// Constant index shape arithmetic remains a valid dimension.
builtin.module {
  %a = "arith.constant"() <{value = 2 : index}> : () -> index
  %b = "arith.constant"() <{value = 3 : index}> : () -> index
  %c = "arith.constant"() <{value = 4 : index}> : () -> index
  %s = "arith.addi"(%a, %b) : (index, index) -> index
  %p = "arith.muli"(%s, %c) : (index, index) -> index
  %u = "test.use"() : () -> !d_tensor.tensor<[%p], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "arith.constant"() <{value = 2 : index}> : () -> index
// CHECK:   %1 = "arith.constant"() <{value = 3 : index}> : () -> index
// CHECK:   %2 = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK:   %3 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CHECK:   %4 = "arith.muli"(%3, %2) {{.*}} : (index, index) -> index
// CHECK:   %5 = "test.use"() : () -> !d_tensor.tensor<[%4], f32>
// CHECK: }

// -----

// d_tensor.dim remains (no dim-fold in strict !value<...> typing mode).
builtin.module {
  %m = "arith.constant"() <{value = 6 : index}> : () -> index
  %n = "arith.constant"() <{value = 9 : index}> : () -> index
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "arith.constant"() <{value = 6 : index}> : () -> index
// CHECK:   %1 = "arith.constant"() <{value = 9 : index}> : () -> index
// CHECK:   %2 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK:   %3 = "d_tensor.dim"(%2) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// CHECK:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%3], f32>
// CHECK: }

// -----

// Must-not-fold: no neutral/constant identities present.
builtin.module {
  %x = "test.index"() : () -> index
  %n = "arith.constant"() <{value = 7 : index}> : () -> index
  %s = "arith.addi"(%x, %n) : (index, index) -> index
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CHECK: builtin.module {
// CHECK:   %0 = "test.index"() : () -> index
// CHECK:   %1 = "arith.constant"() <{value = 7 : index}> : () -> index
// CHECK:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CHECK:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CHECK: }

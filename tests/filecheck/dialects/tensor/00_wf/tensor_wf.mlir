// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY

// Valid: core tensor SSA-shape ops.
builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 8 : index}> : () -> index
  %k = "arith.constant"() <{value = 3 : index}> : () -> index
  %s = "arith.addi"(%m, %k) : (index, index) -> index
  %p = "arith.muli"(%s, %n) : (index, index) -> index
  %z = "test.zero"() : () -> f32
  %a = "d_tensor.fill"(%z) : (f32) -> !d_tensor.tensor<[%m, %k], f32>
  %b = "d_tensor.empty"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %x = "d_tensor.matmul"(%a, %b)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%x) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %v0 = "test.v0"() : () -> !d_tensor.vector<%m, f32>
  %m0 = "test.m0"() : () -> !d_tensor.matrix<%m, %n, f32>
  %c = "d_tensor.cast"(%x)
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 4 : index}> : () -> index
// VERIFY:   %1 = "arith.constant"() <{value = 8 : index}> : () -> index
// VERIFY:   %2 = "arith.constant"() <{value = 3 : index}> : () -> index
// VERIFY:   %3 = "arith.addi"(%0, %2) {{.*}} : (index, index) -> index
// VERIFY:   %4 = "arith.muli"(%3, %1) {{.*}} : (index, index) -> index
// VERIFY:   %5 = "test.zero"() : () -> f32
// VERIFY:   %6 = "d_tensor.fill"(%5) : (f32) -> !d_tensor.tensor<[%0, %2], f32>
// VERIFY:   %7 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%2, %1], f32>
// VERIFY:   %8 = "d_tensor.matmul"(%6, %7) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%2, %1], f32>) -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %9 = "d_tensor.dim"(%8) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// VERIFY:   %10 = "test.v0"() : () -> !d_tensor.vector<%0, f32>
// VERIFY:   %11 = "test.m0"() : () -> !d_tensor.matrix<%0, %1, f32>
// VERIFY:   %12 = "d_tensor.cast"(%8) : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY: }

// -----

// Invalid: dominance-in-types violation with non-dominating dim.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "arith.constant"() <{value = 4 : index}> : () -> index
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %u = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: // -----
// VERIFY: ssa-dominance: value Value(index) does not dominate its use in op `test.use`

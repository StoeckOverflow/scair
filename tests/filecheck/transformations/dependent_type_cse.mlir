// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s

builtin.module {
  %t0 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %t1 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  "test.use"() {dep = !d_tensor.vector<%t1, f32>} : () -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// CHECK-NOT: "d_tensor.nat.const"
// CHECK: "test.use"() {dep = !d_tensor.vector<%0, f32>} : () -> ()
// CHECK: }

// -----

builtin.module {
  %t0 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %a = "arith.constant"() <{value = 1 : i32}> : () -> !d_tensor.vector<%t0, f32>
  %t1 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
  %b = "arith.constant"() <{value = 1 : i32}> : () -> !d_tensor.vector<%t1, f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// CHECK: %1 = "arith.constant"() <{value = 1 : i32}> : () -> !d_tensor.vector<%0, f32>
// CHECK: %2 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// CHECK: %3 = "arith.constant"() <{value = 1 : i32}> : () -> !d_tensor.vector<%2, f32>
// CHECK: }

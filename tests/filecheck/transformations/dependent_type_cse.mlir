// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s

builtin.module {
  %t0 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %t1 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  "test.use"() {dep = !d_tensor.vector<%t1, f32>} : () -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
// CHECK-NOT: "d_tensor.size.constant"
// CHECK: "test.use"() {dep = !d_tensor.vector<%0, f32>} : () -> ()
// CHECK: }

// -----

builtin.module {
  %t0 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %a = "arith.constant"() <{value = 1 : i32}> : () -> !d_tensor.vector<%t0, f32>
  %t1 = "d_tensor.size.constant"() <{value = 1 : i32}> : () -> !d_tensor.size
  %b = "arith.constant"() <{value = 1 : i32}> : () -> !d_tensor.vector<%t1, f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
// CHECK: %1 = "arith.constant"() <{value = 1 : i32}> : () -> !d_tensor.vector<%0, f32>
// CHECK: %2 = "d_tensor.size.constant"() <{value = 1 : i32}> : () -> !d_tensor.size
// CHECK: %3 = "arith.constant"() <{value = 1 : i32}> : () -> !d_tensor.vector<%2, f32>
// CHECK: }

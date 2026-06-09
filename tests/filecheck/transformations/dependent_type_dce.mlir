// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize | filecheck %s

builtin.module {
  %dead = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %used = "d_tensor.size.constant"() <{value = 1 : i32}> : () -> !d_tensor.size
  "test.use"() {dep = !d_tensor.vector<%used, f32>} : () -> ()
}

// CHECK: builtin.module {
// CHECK: %0 = "d_tensor.size.constant"() <{value = 1 : i32}> : () -> !d_tensor.size
// CHECK: "test.use"() {dep = !d_tensor.vector<%0, f32>} : () -> ()
// CHECK: }

// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize | filecheck %s

builtin.module {
  %dead = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %used = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
  "test.use"() {dep = !d_tensor.vector<%used, f32>} : () -> ()
}

// CHECK: builtin.module {
// CHECK: %0 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// CHECK: "test.use"() {dep = !d_tensor.vector<%0, f32>} : () -> ()
// CHECK: }

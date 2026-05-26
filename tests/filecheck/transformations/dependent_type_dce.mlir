// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize | filecheck %s

builtin.module {
  %dead = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %used = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  "test.use"() {dep = !dtensor.vector<%used, f32>} : () -> ()
}

// CHECK: builtin.module {
// CHECK: %0 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// CHECK: "test.use"() {dep = !dtensor.vector<%0, f32>} : () -> ()
// CHECK: }

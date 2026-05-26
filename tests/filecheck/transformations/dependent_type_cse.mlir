// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s

builtin.module {
  %t0 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %t1 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  "test.use"() {dep = !dtensor.vector<%t1, f32>} : () -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// CHECK-NOT: "dtensor.nat.const"
// CHECK: "test.use"() {dep = !dtensor.vector<%0, f32>} : () -> ()
// CHECK: }

// -----

builtin.module {
  %t0 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %a = "arith.constant"() <{value = 1 : i32}> : () -> !dtensor.vector<%t0, f32>
  %t1 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  %b = "arith.constant"() <{value = 1 : i32}> : () -> !dtensor.vector<%t1, f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// CHECK: %1 = "arith.constant"() <{value = 1 : i32}> : () -> !dtensor.vector<%0, f32>
// CHECK: %2 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// CHECK: %3 = "arith.constant"() <{value = 1 : i32}> : () -> !dtensor.vector<%2, f32>
// CHECK: }

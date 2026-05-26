// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p cse | filecheck %s -DFILE=%s

// CSE should RAUW values embedded in dtensor result types, not only ordinary
// operand uses.
builtin.module {
  %m0 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
  %m1 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
  %t = "test.tensor"() : () -> !dtensor.tensor<[%m1], f32>
  "test.keep"(%t) : (!dtensor.tensor<[%m1], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK:   %0 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
// CHECK-NOT: "dtensor.nat.const"
// CHECK:   %1 = "test.tensor"() : () -> !dtensor.tensor<[%0], f32>
// CHECK:   "test.keep"(%1) : (!dtensor.tensor<[%0], f32>) -> ()
// CHECK: }

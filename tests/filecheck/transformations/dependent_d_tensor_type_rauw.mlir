// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p cse | filecheck %s -DFILE=%s

// CSE should RAUW values embedded in d_tensor result types, not only ordinary
// operand uses.
builtin.module {
  %m0 = "d_tensor.size.constant"() <{value = 16 : i32}> : () -> !d_tensor.size
  %m1 = "d_tensor.size.constant"() <{value = 16 : i32}> : () -> !d_tensor.size
  %t = "test.tensor"() : () -> !d_tensor.tensor<[%m1], f32>
  "test.keep"(%t) : (!d_tensor.tensor<[%m1], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK:   %0 = "d_tensor.size.constant"() <{value = 16 : i32}> : () -> !d_tensor.size
// CHECK-NOT: "d_tensor.size.constant"
// CHECK:   %1 = "test.tensor"() : () -> !d_tensor.tensor<[%0], f32>
// CHECK:   "test.keep"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// CHECK: }

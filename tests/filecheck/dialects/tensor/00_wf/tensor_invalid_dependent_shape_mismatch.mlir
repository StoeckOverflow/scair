// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Dedicated negative verifier coverage for dependent shape mismatch.

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %n1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n0], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m, %n0], f32>
  // expected-error @below {{dtensor.add: expected pairwise SSA-identical dims for lhs/result}}
  %bad = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m, %n0], f32>, !dtensor.tensor<[%m, %n0], f32>) -> !dtensor.tensor<[%m, %n1], f32>
}

// CHECK: dtensor.add: expected pairwise SSA-identical dims for lhs/result

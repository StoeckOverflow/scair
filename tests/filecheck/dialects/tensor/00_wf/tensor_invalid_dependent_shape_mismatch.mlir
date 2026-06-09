// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Dedicated negative verifier coverage for dependent shape mismatch.

builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %n1 = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n0], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m, %n0], f32>
  // expected-error @below {{d_tensor.add: expected pairwise SSA-identical dims for lhs/result}}
  %bad = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%m, %n0], f32>, !d_tensor.tensor<[%m, %n0], f32>) -> !d_tensor.tensor<[%m, %n1], f32>
}

// CHECK: d_tensor.add: expected pairwise SSA-identical dims for lhs/result

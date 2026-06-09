// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p tensor-shape-canonicalize,canonicalize,cse,dce 2>&1 | filecheck %s --check-prefix=DIAG

builtin.module {
  func.func @post_pass_bad_result_dims(
    %m : !d_tensor.size,
    %n : !d_tensor.size,
    %lhs : !d_tensor.tensor<[%m, %n], f32>,
    %rhs : !d_tensor.tensor<[%m, %n], f32>
  ) -> !d_tensor.tensor<[%n, %m], f32> {
    // expected-error @below {{d_tensor.add: expected pairwise SSA-identical dims for lhs/result}}
    %bad = "d_tensor.add"(%lhs, %rhs) : (!d_tensor.tensor<[%m, %n], f32>, !d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%n, %m], f32>
    "func.return"(%bad) : (!d_tensor.tensor<[%n, %m], f32>) -> ()
  }
}

// DIAG: d_tensor.add: expected pairwise SSA-identical dims for lhs/result

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p tensor-shape-canonicalize,canonicalize,cse,dce 2>&1 | filecheck %s --check-prefix=DIAG

builtin.module {
  func.func @post_pass_bad_result_dims(
    %m : !dtensor.nat,
    %n : !dtensor.nat,
    %lhs : !dtensor.tensor<[%m, %n], f32>,
    %rhs : !dtensor.tensor<[%m, %n], f32>
  ) -> !dtensor.tensor<[%n, %m], f32> {
    // expected-error @below {{dtensor.add: expected pairwise SSA-identical dims for lhs/result}}
    %bad = "dtensor.add"(%lhs, %rhs) : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%n, %m], f32>
    "func.return"(%bad) : (!dtensor.tensor<[%n, %m], f32>) -> ()
  }
}

// DIAG: dtensor.add: expected pairwise SSA-identical dims for lhs/result

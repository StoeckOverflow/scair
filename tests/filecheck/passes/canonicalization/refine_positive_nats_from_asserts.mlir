// RUN: scair-opt %s --allow-unregistered-dialect -p validate-d-affine-dynamic-steps | filecheck %s

// Strict positivity is now an explicit index fact. A positive constant dynamic
// step is lowerable without Nat refinement.
builtin.module {
  func.func @constant_positive_step_is_valid() {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ub = "arith.constant"() <{value = 16 : index}> : () -> index
    %step = "arith.constant"() <{value = 2 : index}> : () -> index

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step %step : index {
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @constant_positive_step_is_valid
// CHECK: %[[STEP:[0-9]+]] = "arith.constant"() <{value = 2 : index}> : () -> index
// CHECK: d_affine.for %{{[0-9]+}} = {{.*}} step %[[STEP]] : index
// CHECK-NOT: d_tensor.

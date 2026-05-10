// RUN: scair-opt %s --allow-unregistered-dialect -p validate-d-affine-dynamic-steps | filecheck %s

builtin.module {
  func.func @positive_dynamic_step(%step_nat: !dtensor.posnat) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ub = "arith.constant"() <{value = 16 : index}> : () -> index
    %step = "dtensor.shape.to_index"(%step_nat) : (!dtensor.posnat) -> index

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step %step : index {
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @positive_dynamic_step
// CHECK: !dtensor.posnat
// CHECK: step %{{[0-9]+}} : index

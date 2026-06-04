// RUN: ! scair-opt %s --allow-unregistered-dialect -p validate-d-affine-dynamic-steps 2>&1 | filecheck %s

builtin.module {
  func.func @unknown_dynamic_step(%step_nat: !dtensor.nat) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ub = "arith.constant"() <{value = 16 : index}> : () -> index
    %step = "dtensor.shape.to_index"(%step_nat) : (!dtensor.nat) -> index

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step %step : index {
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK: d_affine.for dynamic step must be proven strictly positive before lowering

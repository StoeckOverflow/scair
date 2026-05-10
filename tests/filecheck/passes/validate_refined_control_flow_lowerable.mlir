// RUN: scair-opt %s --allow-unregistered-dialect -p validate-refined-control-flow-lowerable | filecheck %s

builtin.module {
  func.func @lowerable_shifted_bound(%ub: index, %step_nat: !dtensor.posnat) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %step = "dtensor.shape.to_index"(%step_nat) : (!dtensor.posnat) -> index

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0 + 4)>(%ub) step %step : index {
      %next = d_affine.apply affine_map<(d0) -> (d0 * 2)>(%p)[] : (index)[] -> index
      "test.keep"(%next) : (index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @lowerable_shifted_bound
// CHECK: d_affine.for

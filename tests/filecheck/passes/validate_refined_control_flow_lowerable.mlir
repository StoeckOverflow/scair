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

// CHECK: #[[ID:.*]] = affine_map<(d0)[] -> (d0)>
// CHECK: #[[SHIFTED_UB:.*]] = affine_map<(d0)[] -> (d0 + 4)>
// CHECK: #[[DOUBLE:.*]] = affine_map<(d0)[] -> (d0 * 2)>
// CHECK-LABEL: func.func @lowerable_shifted_bound
// CHECK-SAME: (%[[UB:[0-9]+]]: index, %[[STEP_NAT:[0-9]+]]: !dtensor.posnat)
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[STEP:[0-9]+]] = "dtensor.shape.to_index"(%[[STEP_NAT]]) : (!dtensor.posnat) -> index
// CHECK: d_affine.for %[[P:[0-9]+]] = #[[ID]](%[[C0]]) to #[[SHIFTED_UB]](%[[UB]]) step %[[STEP]] : index {
// CHECK-NEXT:   %[[NEXT:[0-9]+]] = d_affine.apply #[[DOUBLE]] (%[[P]])[] : (index)[] -> index
// CHECK-NEXT:   "test.keep"(%[[NEXT]]) : (index) -> ()
// CHECK-NEXT:   d_affine.yield
// CHECK-NEXT: }
// CHECK-NEXT: func.return

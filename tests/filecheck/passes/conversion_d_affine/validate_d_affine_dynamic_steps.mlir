// RUN: scair-opt %s --allow-unregistered-dialect -p validate-d-affine-dynamic-steps | filecheck %s

builtin.module {
  func.func @positive_dynamic_step(%step_size: !d_tensor.pos_size) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ub = "arith.constant"() <{value = 16 : index}> : () -> index

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step %step_size : index {
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK: #[[ID:.*]] = affine_map<(d0)[] -> (d0)>
// CHECK-LABEL: func.func @positive_dynamic_step
// CHECK-SAME: (%[[STEP_NAT:[0-9]+]]: !d_tensor.pos_size)
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[UB:[0-9]+]] = "arith.constant"() <{value = 16 : index}> : () -> index
// CHECK: d_affine.for %[[IV:[0-9]+]] = #[[ID]](%[[C0]]) to #[[ID]](%[[UB]]) step %[[STEP_NAT]] : !d_tensor.pos_size {
// CHECK-NEXT:   d_affine.yield
// CHECK-NEXT: }
// CHECK-NEXT: func.return

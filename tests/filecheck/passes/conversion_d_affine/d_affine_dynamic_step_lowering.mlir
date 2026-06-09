// RUN: scair-opt %s -p lower-refined-control-flow-to-llvm | filecheck %s
// RUN: scair-opt %s -p lower-refined-control-flow-to-llvm | scair-opt --allow-unregistered-dialect --verify-diagnostics

builtin.module {
  func.func @dynamic_step_lowering(%lb: index, %ub: index, %init: index) -> index {
    %step = "arith.constant"() <{value = 2 : index}> : () -> index
    %sum = d_affine.for %iv = affine_map<(d0) -> (d0)>(%lb) to affine_map<(d0) -> (d0)>(%ub) step %step : index iter_args(%acc = %init : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%iv)[%acc] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    func.return %sum : index
  }
}

// CHECK-LABEL: func.func @dynamic_step_lowering
// CHECK: ^bb{{.*}}(%[[IV:.*]]: index, %{{.*}}: index):
// CHECK: "llvm.add"(%[[IV]], %{{.*}}) : (index, index) -> index
// CHECK-NOT: d_affine.apply

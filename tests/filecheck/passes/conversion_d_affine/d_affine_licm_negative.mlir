// RUN: scair-opt %s --allow-unregistered-dialect -p d-affine-loop-invariant-code-motion | filecheck %s

#map = affine_map<(d0)[] -> (d0)>

builtin.module {
  func.func @do_not_hoist_iv_dependent_or_effectful(%lb : index, %ub : index, %stride : index, %offset : index) -> index {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %sum = d_affine.for %i = #map(%lb) to #map(%ub) step 1 : i32 iter_args(%acc = %c0 : index) {
      %inner = d_affine.for %j = #map(%lb) to #map(%ub) step 1 : i32 iter_args(%acc2 = %acc : index) {
        %outer_only = "arith.muli"(%i, %stride) : (index, index) -> index
        %outer_base = "arith.addi"(%outer_only, %offset) : (index, index) -> index
        %iv_dep = "arith.muli"(%j, %stride) : (index, index) -> index
        "test.effect"(%outer_base) : (index) -> ()
        %next = "arith.addi"(%acc2, %iv_dep) : (index, index) -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    func.return %sum : index
  }
}

// CHECK-LABEL: func.func @do_not_hoist_iv_dependent_or_effectful
// CHECK:      d_affine.for %[[I:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1 : i32 iter_args
// CHECK:        %[[OUTER_ONLY:[0-9]+]] = "arith.muli"(%[[I]], %{{[0-9]+}})
// CHECK-NEXT:   %[[OUTER_BASE:[0-9]+]] = "arith.addi"(%[[OUTER_ONLY]], %{{[0-9]+}})
// CHECK-NEXT:   d_affine.for %[[J:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1 : i32 iter_args
// CHECK-NEXT:     %[[IV_DEP:[0-9]+]] = "arith.muli"(%[[J]], %{{[0-9]+}})
// CHECK-NEXT:     "test.effect"(%[[OUTER_BASE]]) : (index) -> ()
// CHECK-NEXT:     %[[NEXT:[0-9]+]] = "arith.addi"(%{{[0-9]+}}, %[[IV_DEP]])
// CHECK-NEXT:     d_affine.yield %[[NEXT]] : (index)

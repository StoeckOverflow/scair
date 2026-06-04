// RUN: scair-opt %s -p d-affine-loop-invariant-code-motion | filecheck %s

#map = affine_map<(d0)[] -> (d0)>

builtin.module {
  func.func @hoist_arith_chain(%lb : index, %ub : index, %stride : index, %offset : index) -> index {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %sum = d_affine.for %i = #map(%lb) to #map(%ub) step 1 : i32 iter_args(%acc = %c0 : index) {
      %inner = d_affine.for %j = #map(%lb) to #map(%ub) step 1 : i32 iter_args(%acc2 = %acc : index) {
        %row = "arith.muli"(%i, %stride) : (index, index) -> index
        %base = "arith.addi"(%row, %offset) : (index, index) -> index
        %addr = "arith.addi"(%base, %j) : (index, index) -> index
        %next = "arith.addi"(%acc2, %addr) : (index, index) -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    func.return %sum : index
  }
}

// CHECK-LABEL: func.func @hoist_arith_chain(%0: index, %1: index, %2: index, %3: index) -> index {
// CHECK-NEXT:    %4 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %5 = d_affine.for %6 = #map(%0) to #map(%1) step 1 : i32 iter_args(%7 = %4 : index) {
// CHECK-NEXT:      %8 = "arith.muli"(%6, %2) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:      %9 = "arith.addi"(%8, %3) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:      %10 = d_affine.for %11 = #map(%0) to #map(%1) step 1 : i32 iter_args(%12 = %7 : index) {
// CHECK-NEXT:        %13 = "arith.addi"(%9, %11) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:        %14 = "arith.addi"(%12, %13) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:        d_affine.yield %14 : (index)
// CHECK-NEXT:      }
// CHECK-NEXT:      d_affine.yield %10 : (index)
// CHECK-NEXT:    }
// CHECK-NEXT:    func.return %5 : index
// CHECK-NEXT:  }

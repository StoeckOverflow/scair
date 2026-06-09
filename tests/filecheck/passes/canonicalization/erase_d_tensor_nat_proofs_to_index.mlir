// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize | filecheck %s

// Final-surface shape arithmetic is already index/arith; no Nat bridge erasure
// pass is needed.
builtin.module {
  func.func @shape_arith_is_already_index(%k0: index, %a: index, %b: index) -> (index, index) {
    %k1 = "arith.constant"() <{value = 4 : index}> : () -> index
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index
    %sum = "arith.addi"(%a, %b) : (index, index) -> index
    "func.return"(%k, %sum) : (index, index) -> ()
  }
}

// CHECK-LABEL: func.func @shape_arith_is_already_index
// CHECK: %[[K1:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK: %[[K:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %[[K1]])
// CHECK: %[[SUM:[0-9]+]] = "arith.addi"(%{{[0-9]+}}, %{{[0-9]+}})
// CHECK: func.return %[[K]], %[[SUM]]
// CHECK-NOT: d_tensor.

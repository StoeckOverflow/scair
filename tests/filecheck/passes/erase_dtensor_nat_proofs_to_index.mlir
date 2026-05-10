// RUN: scair-opt %s --allow-unregistered-dialect -p erase-dtensor-nat-proofs-to-index | filecheck %s

builtin.module {
  func.func @erase_nat_proofs(%k0: index, %out: memref<?xf32>) {
    %k0_nat = "dtensor.index_to_nat"(%k0) : (index) -> !dtensor.nat
    %k1_nat = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %k_nat = "dtensor.nat.mul"(%k0_nat, %k1_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %k = "dtensor.shape.to_index"(%k_nat) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
    }

    "func.return"() : () -> ()
  }

  func.func @erase_nat_add(%a: index, %b: index) -> index {
    %a_nat = "dtensor.index_to_nat"(%a) : (index) -> !dtensor.nat
    %b_nat = "dtensor.index_to_nat"(%b) : (index) -> !dtensor.nat
    %sum_nat = "dtensor.nat.add"(%a_nat, %b_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %sum = "dtensor.shape.to_index"(%sum_nat) : (!dtensor.nat) -> index
    "func.return"(%sum) : (index) -> ()
  }
}

// CHECK-LABEL: func.func @erase_nat_proofs
// CHECK: %[[K1:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK: %[[K:[0-9]+]] = "arith.muli"(%{{[0-9]+}}, %[[K1]])
// CHECK: affine.for %{{[0-9]+}} = #map(%{{[0-9]+}}) to #map(%[[K]]) step 1
// CHECK-LABEL: func.func @erase_nat_add
// CHECK: %[[SUM:[0-9]+]] = "arith.addi"(%{{[0-9]+}}, %{{[0-9]+}})
// CHECK: func.return %[[SUM]]
// CHECK-NOT: dtensor.

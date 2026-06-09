// RUN: scair-opt %s --allow-unregistered-dialect -p erase-d-tensor-nat-proofs-to-index | filecheck %s

builtin.module {
  func.func @erase_nat_proofs(%k0: index, %out: memref<?xf32>) {
    %k0_nat = "d_tensor.index_to_nat"(%k0) : (index) -> !d_tensor.nat
    %k1_nat = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    %k_nat = "d_tensor.nat.mul"(%k0_nat, %k1_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %k = "d_tensor.shape.to_index"(%k_nat) : (!d_tensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
    }

    "func.return"() : () -> ()
  }

  func.func @erase_nat_add(%a: index, %b: index) -> index {
    %a_nat = "d_tensor.index_to_nat"(%a) : (index) -> !d_tensor.nat
    %b_nat = "d_tensor.index_to_nat"(%b) : (index) -> !d_tensor.nat
    %sum_nat = "d_tensor.nat.add"(%a_nat, %b_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %sum = "d_tensor.shape.to_index"(%sum_nat) : (!d_tensor.nat) -> index
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
// CHECK-NOT: d_tensor.

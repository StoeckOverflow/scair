// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile | filecheck %s

builtin.module {
  func.func @dynamic_param_without_positive_assumption(
    %k0_nat: !d_tensor.nat,
    %k1_nat: !d_tensor.nat,
    %out: memref<?xf32>
  ) {
    %k_nat = "d_tensor.nat.mul"(%k0_nat, %k1_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %ub = "d_tensor.shape.to_index"(%k_nat) : (!d_tensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @dynamic_param_without_positive_assumption
// CHECK-SAME: %[[K0:[0-9]+]]: !d_tensor.nat
// CHECK-SAME: %[[K1:[0-9]+]]: !d_tensor.nat
// CHECK-SAME: %[[OUT:[0-9]+]]: memref<?xf32>
// CHECK: %[[K:[0-9]+]] = "d_tensor.nat.mul"(%[[K0]], %[[K1]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CHECK: %[[UB:[0-9]+]] = "d_tensor.shape.to_index"(%[[K]]) : (!d_tensor.nat) -> index
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[CST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[C0]]) to #map(%[[UB]]) step 1 : index {
// CHECK: "memref.store"(%[[CST]], %[[OUT]], %[[P]]) : (f32, memref<?xf32>, index) -> ()

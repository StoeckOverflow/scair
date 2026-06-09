// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile | filecheck %s

builtin.module {
  func.func @static_zero_tile_factor_is_not_used(%out: memref<?xf32>) {
    %k0_nat = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    %k1_nat = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
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

// CHECK-LABEL: func.func @static_zero_tile_factor_is_not_used
// CHECK-SAME: %[[OUT:[0-9]+]]: memref<?xf32>
// CHECK: %[[K4:[0-9]+]] = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// CHECK: %[[K0:[0-9]+]] = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// CHECK: %[[K:[0-9]+]] = "d_tensor.nat.mul"(%[[K4]], %[[K0]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CHECK: %[[UB:[0-9]+]] = "d_tensor.shape.to_index"(%[[K]]) : (!d_tensor.nat) -> index
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[CST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[C0]]) to #map(%[[UB]]) step 1 : index {
// CHECK: "memref.store"(%[[CST]], %[[OUT]], %[[P]]) : (f32, memref<?xf32>, index) -> ()

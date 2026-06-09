// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile | filecheck %s

builtin.module {
  func.func @static_zero_tile_factor_is_not_used(%out: memref<?xf32>) {
    %k0_size = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
    %k1_size = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
    %k_size = "d_tensor.size.mul"(%k0_size, %k1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k_size) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @static_zero_tile_factor_is_not_used
// CHECK-SAME: %[[OUT:[0-9]+]]: memref<?xf32>
// CHECK: %[[K4:[0-9]+]] = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// CHECK: %[[K0:[0-9]+]] = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
// CHECK: %[[K:[0-9]+]] = "d_tensor.size.mul"(%[[K4]], %[[K0]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[CST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[C0]]) to #map(%[[K]]) step 1 : index {
// CHECK: "memref.store"(%[[CST]], %[[OUT]], %[[P]]) : (f32, memref<?xf32>, index) -> ()

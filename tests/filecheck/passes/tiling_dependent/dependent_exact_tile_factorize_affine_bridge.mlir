// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,dependent-size-product-loop-factorization,d-affine-to-affine-compatible | filecheck %s

builtin.module {
  func.func @dynamic_product_exact_tile_factorize_bridge(
    %k0_size: !d_tensor.size,
    %k1_size: !d_tensor.pos_size,
    %out: memref<?xf32>
  ) {
    %k_size = "d_tensor.size.mul"(%k0_size, %k1_size) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k_size) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @dynamic_product_exact_tile_factorize_bridge
// CHECK-SAME: %[[K0:[0-9]+]]: !d_tensor.size
// CHECK-SAME: %[[K1:[0-9]+]]: !d_tensor.pos_size
// CHECK-NOT: d_affine.for
// CHECK: affine.for %[[OI:[0-9]+]] = #map(%{{.*}}) to #map(%[[K0]]) step 1
// CHECK: %[[TILE_START:[0-9]+]] = "arith.muli"(%[[OI]], %[[K1]])
// CHECK: %[[TILE_END:[0-9]+]] = "affine.apply"(%[[TILE_START]], %[[K1]])
// CHECK: affine.for %[[P:[0-9]+]] = #map(%[[TILE_START]]) to #map(%[[TILE_END]]) step 1
// CHECK: "memref.store"({{.*}}, {{.*}}, %[[P]])
// CHECK-NOT: d_affine.for
// CHECK-NOT: step %
// CHECK-NOT: arith.minsi
// CHECK-NOT: to min

// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,dependent-natmul-loop-factorization,d-affine-to-affine-compatible | filecheck %s

builtin.module {
  func.func @dynamic_product_exact_tile_factorize_bridge(
    %k0_nat: !dtensor.nat,
    %k1_nat: !dtensor.posnat,
    %out: memref<?xf32>
  ) {
    %k_nat = "dtensor.nat.mul"(%k0_nat, %k1_nat) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
    %ub = "dtensor.shape.to_index"(%k_nat) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @dynamic_product_exact_tile_factorize_bridge
// CHECK-SAME: %[[K0:[0-9]+]]: !dtensor.nat
// CHECK-SAME: %[[K1:[0-9]+]]: !dtensor.posnat
// CHECK-NOT: d_affine.for
// CHECK: %[[OUTER_UB:[0-9]+]] = "dtensor.shape.to_index"(%[[K0]]) : (!dtensor.nat) -> index
// CHECK: %[[INNER_UB:[0-9]+]] = "dtensor.shape.to_index"(%[[K1]]) : (!dtensor.posnat) -> index
// CHECK: affine.for %[[OI:[0-9]+]] = #map(%{{.*}}) to #map(%[[OUTER_UB]]) step 1
// CHECK: %[[TILE_START:[0-9]+]] = "arith.muli"(%[[OI]], %[[INNER_UB]])
// CHECK: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE_START]], %{{[0-9]+}})
// CHECK: affine.for %[[P:[0-9]+]] = #map(%[[TILE_START]]) to #map(%[[TILE_END]]) step 1
// CHECK: "memref.store"({{.*}}, {{.*}}, %[[P]])
// CHECK-NOT: d_affine.for
// CHECK-NOT: step %
// CHECK-NOT: arith.minsi
// CHECK-NOT: to min

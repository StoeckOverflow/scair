// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,dependent-natmul-loop-factorization | filecheck %s

builtin.module {
  func.func @dynamic_product_exact_tile_factorize(
    %k0_nat: !d_tensor.nat,
    %k1_nat: !d_tensor.posnat,
    %out: memref<?xf32>
  ) {
    %k_nat = "d_tensor.nat.mul"(%k0_nat, %k1_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
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

// CHECK-LABEL: func.func @dynamic_product_exact_tile_factorize
// CHECK-SAME: %[[K0:[0-9]+]]: !d_tensor.nat
// CHECK-SAME: %[[K1:[0-9]+]]: !d_tensor.posnat
// CHECK: %[[OUTER_UB:[0-9]+]] = "d_tensor.shape.to_index"(%[[K0]]) : (!d_tensor.nat) -> index
// CHECK: %[[INNER_UB:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1]]) : (!d_tensor.posnat) -> index
// CHECK: d_affine.for %[[OI:[0-9]+]] = #map(%{{.*}}) to #map(%[[OUTER_UB]]) step 1 : i32
// CHECK: %[[TILE_START:[0-9]+]] = "arith.muli"(%[[OI]], %[[INNER_UB]])
// CHECK: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE_START]], %{{[0-9]+}})
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE_START]]) to #map(%[[TILE_END]]) step 1 : i32
// CHECK: "memref.store"({{.*}}, {{.*}}, %[[P]])
// CHECK-NOT: step %
// CHECK-NOT: arith.minsi

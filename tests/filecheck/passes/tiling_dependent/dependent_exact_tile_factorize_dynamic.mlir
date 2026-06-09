// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,dependent-product-loop-factorization | filecheck %s

builtin.module {
  func.func @dynamic_product_exact_tile_factorize(
    %k0: index,
    %out: memref<?xf32>
  ) {
    %k1 = "arith.constant"() <{value = 8 : index}> : () -> index
    %ub = "arith.muli"(%k0, %k1) : (index, index) -> index
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
// CHECK-SAME: %[[K0:[0-9]+]]: index
// CHECK: %[[INNER_UB:[0-9]+]] = "arith.constant"() <{value = 8 : index}> : () -> index
// CHECK: d_affine.for %[[OI:[0-9]+]] = #map(%{{.*}}) to #map(%[[K0]]) step 1 : i32
// CHECK: %[[TILE_START:[0-9]+]] = "arith.muli"(%[[OI]], %[[INNER_UB]])
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE_START]]) to #map{{[0-9]+}}(%[[TILE_START]]) step 1 : i32
// CHECK: "memref.store"({{.*}}, {{.*}}, %[[P]])
// CHECK-NOT: step %
// CHECK-NOT: arith.minsi

// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,d-affine-to-affine-compatible | filecheck %s

builtin.module {
  func.func @static_exact_tile_bridge(%k0_nat: !d_tensor.nat, %out: memref<?xf32>) {
    %k1_nat = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
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

// CHECK-LABEL: func.func @static_exact_tile_bridge
// CHECK: affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step 8
// CHECK: affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map{{[0-9]+}}(%[[TILE]]) step 1
// CHECK: "memref.store"({{.*}}, {{.*}}, %[[P]])
// CHECK-NOT: d_affine.for
// CHECK-NOT: arith.addi
// CHECK-NOT: step %
// CHECK-NOT: arith.minsi
// CHECK-NOT: to min

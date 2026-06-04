// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile | filecheck %s

builtin.module {
  %k0 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
  %k1 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %k = "dtensor.nat.mul"(%k0, %k1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %ub = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
  %out = "test.memref"() : () -> memref<?xf32>
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

  d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
    "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
    d_affine.yield
  }
}

// CHECK: #map = affine_map<(d0)[] -> (d0)>
// CHECK: #map1 = affine_map<(d0)[] -> (d0 + 3)>
// CHECK: %[[K0:[0-9]+]] = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
// CHECK: %[[K1:[0-9]+]] = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
// CHECK: %[[K:[0-9]+]] = "dtensor.nat.mul"(%[[K0]], %[[K1]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK: %[[UB:[0-9]+]] = "dtensor.shape.to_index"(%[[K]]) : (!dtensor.nat) -> index
// CHECK: %[[OUT:[0-9]+]] = "test.memref"() : () -> memref<?xf32>
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[CST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// CHECK: %[[TILE_C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[TILE_SIZE:[0-9]+]] = "dtensor.shape.to_index"(%[[K1]]) : (!dtensor.nat) -> index
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%[[TILE_C0]]) to #map(%[[UB]]) step 3 : i32 {
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map{{[0-9]+}}(%[[TILE]]) step 1 : i32
// CHECK: "memref.store"(%[[CST]], %[[OUT]], %[[P]]) : (f32, memref<?xf32>, index) -> ()
// CHECK-NOT: arith.addi
// CHECK-NOT: arith.minsi
// CHECK-NOT: to min
// CHECK-NOT: d_affine.min
// CHECK-NOT: remainder
// CHECK-NOT: mod

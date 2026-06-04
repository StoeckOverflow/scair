// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tile-with-tail-control,dependent-tail-min-simplify,canonicalize,cse,dce | filecheck %s

builtin.module {
  %k0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %k1 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %k = "dtensor.nat.mul"(%k0, %k1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K1:[0-9]+]] = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
// CHECK: %[[TILE_SIZE:[0-9]+]] = "dtensor.shape.to_index"(%[[K1]]) : (!dtensor.nat) -> index
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step 8 : i32 iter_args
// CHECK: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[TILE_SIZE]])
// CHECK-NOT: arith.minsi
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : i32 iter_args

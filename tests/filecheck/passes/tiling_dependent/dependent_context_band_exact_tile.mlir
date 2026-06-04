// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-context-band-exact-tile | filecheck %s

builtin.module {
  %m0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %tile_nat = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %m_nat = "dtensor.nat.mul"(%m0, %tile_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %m = "dtensor.shape.to_index"(%m_nat) : (!dtensor.nat) -> index

  d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
    "test.touch"(%i) : (index) -> ()
    d_affine.yield
  }
}

// CHECK: %[[TILE_NAT:[0-9]+]] = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// CHECK: %[[TILE:[0-9]+]] = "dtensor.shape.to_index"(%[[TILE_NAT]]) : (!dtensor.nat) -> index
// CHECK: d_affine.for %[[OUTER:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %[[TILE]] : index
// CHECK: %[[END:[0-9]+]] = "arith.addi"(%[[OUTER]], %[[TILE]])
// CHECK: d_affine.for %[[INNER:[0-9]+]] = #map(%[[OUTER]]) to #map(%[[END]]) step 1 : index
// CHECK: "test.touch"(%[[INNER]])
// CHECK-NOT: arith.minsi
// CHECK-NOT: to min

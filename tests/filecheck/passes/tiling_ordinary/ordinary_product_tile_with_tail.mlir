// RUN: scair-opt %s --allow-unregistered-dialect -p ordinary-product-tile-with-tail | filecheck %s

builtin.module {
  %k0 = "test.arg"() : () -> index
  %k1_nat = "dtensor.nat.param"() : () -> !dtensor.posnat
  %k1 = "dtensor.shape.to_index"(%k1_nat) : (!dtensor.posnat) -> index
  %k = "arith.muli"(%k0, %k1) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K0:[0-9]+]] = "test.arg"() : () -> index
// CHECK: %[[K1_NAT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.posnat
// CHECK: %[[K1:[0-9]+]] = "dtensor.shape.to_index"(%[[K1_NAT]]) : (!dtensor.posnat) -> index
// CHECK: %[[K:[0-9]+]] = "arith.muli"(%[[K0]], %[[K1]])
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%[[K]]) step %[[K1]] : index iter_args
// CHECK: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[K1]])
// CHECK: %[[CLAMPED:[0-9]+]] = "arith.minsi"(%[[TILE_END]], %[[K]])
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32 iter_args
// CHECK: d_affine.apply {{.*}}(%[[P]])
// CHECK-NOT: dtensor.nat.mul

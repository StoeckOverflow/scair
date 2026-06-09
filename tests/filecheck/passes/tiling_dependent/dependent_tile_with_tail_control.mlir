// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tile-with-tail-control | filecheck %s

builtin.module {
  %k0 = "arith.constant"() <{value = 4 : index}> : () -> index
  %k1 = "arith.constant"() <{value = 8 : index}> : () -> index
  %ub = "arith.muli"(%k0, %k1) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K1:[0-9]+]] = "arith.constant"() <{value = 8 : index}> : () -> index
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step 8 : i32 iter_args
// CHECK: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[K1]])
// CHECK: %[[CLAMPED:[0-9]+]] = "arith.minsi"(%[[TILE_END]], %{{.*}})
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32 iter_args
// CHECK: d_affine.apply {{.*}}(%[[P]])

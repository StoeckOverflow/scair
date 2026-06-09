// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s

builtin.module {
  %k0 = "test.index"() : () -> index
  %k1 = "test.index"() : () -> index
  %full = "arith.muli"(%k0, %k1) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %step = "arith.addi"(%k1, %c0) : (index, index) -> index
  %tile_size = "arith.addi"(%k1, %c0) : (index, index) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
    %tile_end = "arith.addi"(%tile_size, %tile) : (index, index) -> index
    %clamped = "arith.minsi"(%tile_end, %full) : (index, index) -> index
    %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc1] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    d_affine.yield %inner : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[STEP:[0-9]+]] = "arith.addi"(%{{[0-9]+}}, %{{[0-9]+}}) {{.*}} : (index, index) -> index
// CHECK: %[[TILE_SIZE:[0-9]+]] = "arith.addi"(%{{[0-9]+}}, %{{[0-9]+}}) {{.*}} : (index, index) -> index
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %[[STEP]] : index iter_args
// CHECK: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE_SIZE]], %[[TILE]])
// CHECK: %[[CLAMP:[0-9]+]] = "arith.minsi"(%[[TILE_END]], %{{[0-9]+}}) : (index, index) -> index
// CHECK: d_affine.for %{{[0-9]+}} = #map(%[[TILE]]) to #map(%[[CLAMP]]) step 1 : index iter_args

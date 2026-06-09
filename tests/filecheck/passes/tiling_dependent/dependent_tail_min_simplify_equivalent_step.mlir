// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s

builtin.module {
  %k0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %k1 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
  %k = "d_tensor.nat.mul"(%k0, %k1) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %full = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
  %step = "d_tensor.shape.to_index"(%k1) : (!d_tensor.posnat) -> index
  %tile_size = "d_tensor.shape.to_index"(%k1) : (!d_tensor.posnat) -> index
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

// CHECK: %[[K1:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.posnat
// CHECK: %[[STEP:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1]]) : (!d_tensor.posnat) -> index
// CHECK: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1]]) : (!d_tensor.posnat) -> index
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %[[STEP]] : index iter_args
// CHECK: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE_SIZE]], %[[TILE]])
// CHECK-NOT: arith.minsi
// CHECK: d_affine.for %{{[0-9]+}} = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args

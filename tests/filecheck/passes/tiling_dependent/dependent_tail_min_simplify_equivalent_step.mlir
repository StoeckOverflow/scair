// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s

builtin.module {
  %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %k1 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
  %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step %k1 : index iter_args(%acc0 = %init : index) {
    %tile_end = d_affine.apply affine_map<(d0)[s0] -> (s0 + d0)>(%tile)[%k1] : (index)[!d_tensor.pos_size] -> index
    %clamped = d_affine.min affine_map<(d0)[s0] -> (d0, s0)>(%tile_end)[%k] : (index)[!d_tensor.size] -> index
    %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc1] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    d_affine.yield %inner : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K1:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %[[K1]] : !d_tensor.pos_size iter_args
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[K1]]] : (index)[!d_tensor.pos_size] -> index
// CHECK-NOT: arith.minsi
// CHECK: %[[CLAMPED:[0-9]+]] = d_affine.min #map{{[0-9]*}} (%[[TILE_END]])[%{{[0-9]+}}] : (index)[!d_tensor.size] -> index
// CHECK: d_affine.for %{{[0-9]+}} = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : index iter_args

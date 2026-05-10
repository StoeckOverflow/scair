// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s

builtin.module {
  %k0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %k1 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %other = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.mul"(%k0, %k1) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %full = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
  %bad_full = "dtensor.shape.to_index"(%other) : (!dtensor.nat) -> index
  %step = "dtensor.shape.to_index"(%k1) : (!dtensor.posnat) -> index
  %tile_size = "dtensor.shape.to_index"(%k1) : (!dtensor.posnat) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
    %clamped = d_affine.min affine_map<(d0)[s0, s1] -> (d0 + s0, s1)>(%tile)[%tile_size, %bad_full] : (index)[index, index] -> index
    %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc1] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    d_affine.yield %inner : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: d_affine.min
// CHECK: to #map(%{{[0-9]+}}) step 1 : index iter_args

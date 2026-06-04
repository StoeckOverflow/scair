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

// CHECK: #[[ID:.*]] = affine_map<(d0)[] -> (d0)>
// CHECK: #[[BAD_MIN:.*]] = affine_map<(d0)[s0, s1] -> (d0 + s0, s1)>
// CHECK: #[[ADD_ACC:.*]] = affine_map<(d0)[s0] -> (d0 + s0)>
// CHECK: %[[K0:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %[[K1:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.posnat
// CHECK: %[[OTHER:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %[[K:[0-9]+]] = "dtensor.nat.mul"(%[[K0]], %[[K1]]) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
// CHECK: %[[FULL:[0-9]+]] = "dtensor.shape.to_index"(%[[K]]) : (!dtensor.nat) -> index
// CHECK: %[[BAD_FULL:[0-9]+]] = "dtensor.shape.to_index"(%[[OTHER]]) : (!dtensor.nat) -> index
// CHECK: %[[STEP:[0-9]+]] = "dtensor.shape.to_index"(%[[K1]]) : (!dtensor.posnat) -> index
// CHECK: %[[TILE_SIZE:[0-9]+]] = "dtensor.shape.to_index"(%[[K1]]) : (!dtensor.posnat) -> index
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #[[ID]](%{{[0-9]+}}) to #[[ID]](%[[FULL]]) step %[[STEP]] : index iter_args(%[[ACC0:[0-9]+]] = %{{[0-9]+}} : index) {
// CHECK: %[[CLAMPED:[0-9]+]] = d_affine.min #[[BAD_MIN]] (%[[TILE]])[%[[TILE_SIZE]], %[[BAD_FULL]]] : (index)[index, index] -> index
// CHECK: %[[INNER:[0-9]+]] = d_affine.for %[[P:[0-9]+]] = #[[ID]](%[[TILE]]) to #[[ID]](%[[CLAMPED]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: %[[NEXT:[0-9]+]] = d_affine.apply #[[ADD_ACC]] (%[[P]])[%[[ACC1]]] : (index)[index] -> index
// CHECK: d_affine.yield %[[NEXT]] : (index)
// CHECK: d_affine.yield %[[INNER]] : (index)
// CHECK: "test.keep"(%[[SUM]]) : (index) -> ()

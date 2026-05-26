// RUN: scair-opt %s --allow-unregistered-dialect -p refine-positive-nats-from-asserts,dependent-product-loop-separable-tile | filecheck %s

builtin.module {
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %k_check = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ok = "arith.cmpi"(%k_check, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
  "cf.assert"(%ok) <{msg = "k must be positive"}> : (i1) -> ()
  %k_idx = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
  %n_idx = "dtensor.shape.to_index"(%n) : (!dtensor.nat) -> index
  %ub = "arith.muli"(%n_idx, %k_idx) : (index, index) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    d_affine.yield %acc : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K_POS:[0-9]+]] = "dtensor.nat.refine_positive"
// CHECK: %[[K_IDX:[0-9]+]] = "dtensor.shape.to_index"(%[[K_POS]]) : (!dtensor.posnat) -> index
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %[[K_IDX]] : index iter_args
// CHECK: "d_affine.if"(%[[TILE]], %{{[0-9]+}}, %[[K_IDX]]) <{condition = #set}> ({
// CHECK: d_affine.yield
// CHECK: }, {
// CHECK: arith.minsi
// CHECK: d_affine.yield
// CHECK: }) : (index, index, index) -> index

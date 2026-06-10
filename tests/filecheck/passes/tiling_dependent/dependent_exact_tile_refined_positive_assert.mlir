// RUN: scair-opt %s --allow-unregistered-dialect -p refine-positive-nats-from-asserts,dependent-exact-tile | filecheck %s

builtin.module {
  %k0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %k1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %k1_idx = "d_tensor.shape.to_index"(%k1) : (!d_tensor.nat) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ok = "arith.cmpi"(%k1_idx, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
  "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
  %k = "d_tensor.nat.mul"(%k0, %k1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %ub = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K1_POS:[0-9]+]] = "d_tensor.nat.refine_positive"
// CHECK: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1_POS]]) : (!d_tensor.posnat) -> index
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step %[[TILE_SIZE]] : index iter_args
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%{{[0-9]+}}) step 1 : i32 iter_args
// CHECK-NOT: arith.minsi

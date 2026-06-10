// RUN: scair-opt %s --allow-unregistered-dialect -p refine-positive-size-witnesses-from-asserts,dependent-exact-tile | filecheck %s

builtin.module {
  %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %k1_idx = "arith.constant"() <{value = 8 : index}> : () -> index
  %k1 = "d_tensor.size.import"(%k1_idx) : (index) -> !d_tensor.size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ok = "arith.cmpi"(%k1_idx, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
  "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
  %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K1_POS:[0-9]+]] = "d_tensor.size.refine_positive"
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step %[[K1_POS]] : !d_tensor.pos_size iter_args
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%{{[0-9]+}}) step 1 : i32 iter_args
// CHECK-NOT: arith.minsi

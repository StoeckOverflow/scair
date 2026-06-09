// RUN: scair-opt %s --allow-unregistered-dialect -p refine-positive-size-witnesses-from-asserts,dependent-product-loop-separable-tile | filecheck %s

builtin.module {
  %k_idx = "arith.constant"() <{value = 8 : index}> : () -> index
  %k = "d_tensor.size.import"(%k_idx) : (index) -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ok = "arith.cmpi"(%k_idx, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
  "cf.assert"(%ok) <{msg = "k must be positive"}> : (i1) -> ()
  %ub = "d_tensor.size.mul"(%n, %k) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    d_affine.yield %acc : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K_POS:[0-9]+]] = "d_tensor.size.refine_positive"
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %{{[0-9]+}} : !d_tensor.pos_size iter_args
// CHECK-NOT: d_affine.if
// CHECK: d_affine.apply
// CHECK: d_affine.for

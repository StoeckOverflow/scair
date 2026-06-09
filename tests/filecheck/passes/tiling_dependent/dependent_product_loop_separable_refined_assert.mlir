// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-separable-tile | filecheck %s

builtin.module {
  %k = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ok = "arith.cmpi"(%k, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
  "cf.assert"(%ok) <{msg = "k must be positive"}> : (i1) -> ()
  %ub = "arith.muli"(%n, %k) : (index, index) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    d_affine.yield %acc : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: "cf.assert"
// CHECK: %[[UB:[0-9]+]] = "arith.muli"
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%[[UB]]) step 1 : index iter_args
// CHECK-NOT: d_affine.if
// CHECK-NOT: arith.minsi

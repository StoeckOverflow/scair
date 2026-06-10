// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-exact-tile | filecheck %s

builtin.module {
  %k0 = "test.index"() : () -> index
  %k1 = "test.index"() : () -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ok = "arith.cmpi"(%k1, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
  "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
  %ub = "arith.muli"(%k0, %k1) : (index, index) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: "cf.assert"
// CHECK: %[[UB:[0-9]+]] = "arith.muli"
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%[[UB]]) step %{{[0-9]+}} : index iter_args
// CHECK: "arith.addi"(%[[TILE]], %{{[0-9]+}})
// CHECK: d_affine.for %[[P:[0-9]+]] = #map{{[0-9]*}}(%[[TILE]]) to #map{{[0-9]*}}(%{{[0-9]+}}) step 1 : i32 iter_args
// CHECK-NOT: arith.minsi

// RUN: scair-opt %s --allow-unregistered-dialect -p ordinary-product-tile-with-tail | filecheck %s

builtin.module {
  %k0 = "test.arg"() : () -> index
  %k1 = "test.arg"() : () -> index
  %k = "arith.muli"(%k0, %k1) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K0:[0-9]+]] = "test.arg"() : () -> index
// CHECK: %[[K1:[0-9]+]] = "test.arg"() : () -> index
// CHECK: %[[K:[0-9]+]] = "arith.muli"(%[[K0]], %[[K1]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[INIT:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[P:[0-9]+]] = #map(%[[C0]]) to #map(%[[K]]) step 1 : index iter_args(%[[ACC:[0-9]+]] = %[[INIT]] : index) {
// CHECK: %[[NEXT:[0-9]+]] = d_affine.apply #map1 (%[[P]])[%[[ACC]]] : (index)[index] -> index
// CHECK: d_affine.yield %[[NEXT]] : (index)
// CHECK: "test.keep"(%[[SUM]]) : (index) -> ()

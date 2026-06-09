// RUN: scair-opt %s --allow-unregistered-dialect -p ordinary-affine-product-tile-with-tail:4 | filecheck %s

builtin.module {
  %k0 = "test.arg"() : () -> index
  %k1 = "test.arg"() : () -> index
  %k = "arith.muli"(%k0, %k1) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %init : index) {
    %next = "arith.addi"(%p, %acc) : (index, index) -> index
    affine.yield %next : index
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K0:[0-9]+]] = "test.arg"() : () -> index
// CHECK: %[[K1:[0-9]+]] = "test.arg"() : () -> index
// CHECK: %[[K:[0-9]+]] = "arith.muli"(%[[K0]], %[[K1]])
// CHECK-NOT: d_tensor
// CHECK-NOT: d_affine
// CHECK-NOT: d_memref
// CHECK: affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map{{[0-9]*}}()[%[[K]]] step 4 iter_args
// CHECK: affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to min #map{{[0-9]*}}(%[[TILE]])[%[[K]]] step 1 iter_args
// CHECK: "arith.addi"(%[[P]]

// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-factorization | filecheck %s

builtin.module {
  %outer = "arith.constant"() <{value = 4 : index}> : () -> index
  %inner = "arith.constant"() <{value = 8 : index}> : () -> index
  %flat = "arith.muli"(%outer, %inner) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%flat) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[OUTER:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK: %[[INNER:[0-9]+]] = "arith.constant"() <{value = 8 : index}> : () -> index
// CHECK: d_affine.for %[[OI:[0-9]+]] = #map(%{{.*}}) to #map(%[[OUTER]]) step 1 : i32 iter_args
// CHECK: d_affine.for %[[II:[0-9]+]] = #map(%{{.*}}) to #map(%[[INNER]]) step 1 : i32 iter_args
// CHECK: %[[MUL:[0-9]+]] = "arith.muli"(%[[OI]], %[[INNER]])
// CHECK: %[[FLAT:[0-9]+]] = "arith.addi"(%[[MUL]], %[[II]])
// CHECK: d_affine.apply {{.*}}(%[[FLAT]])

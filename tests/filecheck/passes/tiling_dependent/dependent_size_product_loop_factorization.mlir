// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-size-product-loop-factorization | filecheck %s

builtin.module {
  %outer_size = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %inner_size = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
  %flat_size = "d_tensor.size.mul"(%outer_size, %inner_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%flat_size) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[OUTER_NAT:[0-9]+]] = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// CHECK: %[[INNER_NAT:[0-9]+]] = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
// CHECK: d_affine.for %[[OI:[0-9]+]] = #map(%{{.*}}) to #map(%[[OUTER_NAT]]) step 1 : i32 iter_args
// CHECK: d_affine.for %[[II:[0-9]+]] = #map(%{{.*}}) to #map(%[[INNER_NAT]]) step 1 : i32 iter_args
// CHECK: %[[MUL:[0-9]+]] = "arith.muli"(%[[OI]], %[[INNER_NAT]])
// CHECK: %[[FLAT:[0-9]+]] = "arith.addi"(%[[MUL]], %[[II]])
// CHECK: d_affine.apply {{.*}}(%[[FLAT]],

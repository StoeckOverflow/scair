// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-natmul-loop-factorization | filecheck %s

builtin.module {
  %outer_nat = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %inner_nat = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
  %flat_nat = "d_tensor.nat.mul"(%outer_nat, %inner_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %flat = "d_tensor.shape.to_index"(%flat_nat) : (!d_tensor.nat) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%flat) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[OUTER_NAT:[0-9]+]] = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// CHECK: %[[INNER_NAT:[0-9]+]] = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
// CHECK: %[[OUTER:[0-9]+]] = "d_tensor.shape.to_index"(%[[OUTER_NAT]]) : (!d_tensor.nat) -> index
// CHECK: %[[INNER:[0-9]+]] = "d_tensor.shape.to_index"(%[[INNER_NAT]]) : (!d_tensor.nat) -> index
// CHECK: d_affine.for %[[OI:[0-9]+]] = #map(%{{.*}}) to #map(%[[OUTER]]) step 1 : i32 iter_args
// CHECK: d_affine.for %[[II:[0-9]+]] = #map(%{{.*}}) to #map(%[[INNER]]) step 1 : i32 iter_args
// CHECK: %[[MUL:[0-9]+]] = "arith.muli"(%[[OI]], %[[INNER]])
// CHECK: %[[FLAT:[0-9]+]] = "arith.addi"(%[[MUL]], %[[II]])
// CHECK: d_affine.apply {{.*}}(%[[FLAT]],

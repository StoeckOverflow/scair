// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-exact-tile | filecheck %s

builtin.module {
  %k0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %k1 = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
  %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// CHECK: %[[K1:[0-9]+]] = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step 8 : i32 iter_args
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map{{[0-9]+}}(%[[TILE]]) step 1 : i32 iter_args
// CHECK: d_affine.apply {{.*}}(%[[P]],
// CHECK-NOT: arith.addi
// CHECK-NOT: arith.minsi
// CHECK-NOT: to min
// CHECK-NOT: d_affine.min
// CHECK-NOT: remainder
// CHECK-NOT: mod

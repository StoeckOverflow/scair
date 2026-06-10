// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tile-with-tail-control,dependent-tail-min-simplify,canonicalize,cse,dce | filecheck %s

builtin.module {
  %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
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
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[K1]]] : (index)[!d_tensor.size] -> index
// CHECK: %[[CLAMPED:[0-9]+]] = d_affine.min #map{{[0-9]*}} (%[[TILE_END]])[%{{[0-9]+}}] : (index)[!d_tensor.size] -> index
// CHECK-NOT: arith.minsi
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32 iter_args

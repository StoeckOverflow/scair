// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tile-with-tail-control | filecheck %s --check-prefix=GUARDED
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tile-with-tail-control,dependent-tail-min-simplify,canonicalize,cse,dce | filecheck %s --check-prefix=SIMPLIFIED

builtin.module {
  %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %k1 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
  %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// GUARDED: %[[K1:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// GUARDED: %[[K:[0-9]+]] = "d_tensor.size.mul"(%{{[0-9]+}}, %[[K1]]) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
// GUARDED: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%[[K]]) step %[[K1]] : !d_tensor.pos_size iter_args
// GUARDED: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[K1]]] : (index)[!d_tensor.pos_size] -> index
// GUARDED: %[[CLAMPED:[0-9]+]] = d_affine.min #map{{[0-9]*}} (%[[TILE_END]])[%[[K]]] : (index)[!d_tensor.size] -> index
// GUARDED: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32 iter_args

// SIMPLIFIED: %[[K1:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// SIMPLIFIED: %[[K:[0-9]+]] = "d_tensor.size.mul"(%{{[0-9]+}}, %[[K1]]) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
// SIMPLIFIED: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%[[K]]) step %[[K1]] : !d_tensor.pos_size iter_args
// SIMPLIFIED: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[K1]]] : (index)[!d_tensor.pos_size] -> index
// SIMPLIFIED-NOT: arith.minsi
// SIMPLIFIED: %[[CLAMPED:[0-9]+]] = d_affine.min #map{{[0-9]*}} (%[[TILE_END]])[%[[K]]] : (index)[!d_tensor.size] -> index
// SIMPLIFIED: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32 iter_args

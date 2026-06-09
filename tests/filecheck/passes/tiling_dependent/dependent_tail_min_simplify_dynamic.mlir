// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tile-with-tail-control | filecheck %s --check-prefix=GUARDED
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tile-with-tail-control,dependent-tail-min-simplify,canonicalize,cse,dce | filecheck %s --check-prefix=SIMPLIFIED

builtin.module {
  %k0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %k1 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
  %k = "d_tensor.nat.mul"(%k0, %k1) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

// GUARDED: d_tensor.nat.mul
// GUARDED: %[[FULL_BOUND:[0-9]+]] = "d_tensor.shape.to_index"(%{{[0-9]+}}) : (!d_tensor.nat) -> index
// GUARDED: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%{{[0-9]+}}) : (!d_tensor.posnat) -> index
// GUARDED: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step %[[TILE_SIZE]] : index iter_args
// GUARDED: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[TILE_SIZE]])
// GUARDED: %[[CLAMPED:[0-9]+]] = "arith.minsi"(%[[TILE_END]], %{{.*}})
// GUARDED: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : i32 iter_args

// SIMPLIFIED: d_tensor.nat.mul
// SIMPLIFIED: %[[FULL_BOUND:[0-9]+]] = "d_tensor.shape.to_index"(%{{[0-9]+}}) : (!d_tensor.nat) -> index
// SIMPLIFIED: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%{{[0-9]+}}) : (!d_tensor.posnat) -> index
// SIMPLIFIED: d_affine.for %[[TILE:[0-9]+]] = #map(%{{.*}}) to #map(%{{.*}}) step %[[TILE_SIZE]] : index iter_args
// SIMPLIFIED: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[TILE_SIZE]])
// SIMPLIFIED-NOT: arith.minsi
// SIMPLIFIED: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : i32 iter_args

// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-dtensor-nat-products,dependent-context-band-exact-tile,canonicalize,cse,dce | filecheck %s

builtin.module {
  func.func @context_band_does_not_tile_reduction(%init: f32) -> f32 {
    %m0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %tile_nat = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %m_nat = "dtensor.nat.mul"(%m0, %tile_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %m = "dtensor.shape.to_index"(%m_nat) : (!dtensor.nat) -> index

    d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index {
      %sum = d_affine.for %k = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%m) step 1 : index iter_args(%acc = %init : f32) {
        d_affine.yield %acc : (f32)
      }
      "test.touch"(%i, %sum) : (index, f32) -> ()
      d_affine.yield
    }
    func.return %init : f32
  }
}

// CHECK-LABEL: func.func @context_band_does_not_tile_reduction
// CHECK: d_affine.for %[[OUTER_TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step %{{[0-9]+}} : index
// CHECK: d_affine.for %[[I:[0-9]+]] = #map(%[[OUTER_TILE]]) to #map(%{{[0-9]+}}) step 1 : index
// CHECK: %{{[0-9]+}} = d_affine.for %[[K:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 1 : index iter_args
// CHECK: "test.touch"(%[[I]], %{{[0-9]+}})
// CHECK-NOT: d_affine.if

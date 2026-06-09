// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-nat-products,dependent-context-band-exact-tile,canonicalize,cse,dce | filecheck %s

builtin.module {
  func.func @context_band_does_not_tile_reduction(%init: f32) -> f32 {
    %m0 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
    %tile_nat = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    %m_nat = "d_tensor.nat.mul"(%m0, %tile_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %m = "d_tensor.shape.to_index"(%m_nat) : (!d_tensor.nat) -> index

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

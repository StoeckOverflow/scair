// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-separable-tile | filecheck %s

builtin.module {
  func.func @ordinary_product_gets_separable_tile(%n: index, %init: f32) -> f32 {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c4 = "arith.constant"() <{value = 4 : index}> : () -> index
    %ub = "arith.muli"(%n, %c4) : (index, index) -> index
    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : f32) {
      d_affine.yield %acc : (f32)
    }
    func.return %sum : f32
  }

  func.func @shape_product_prefers_exact(%init: f32) -> f32 {
    %n0 = "arith.constant"() <{value = 3 : index}> : () -> index
    %tile = "arith.constant"() <{value = 4 : index}> : () -> index
    %n = "arith.muli"(%n0, %tile) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%acc = %init : f32) {
      d_affine.yield %acc : (f32)
    }
    func.return %sum : f32
  }
}

// CHECK-LABEL: func.func @ordinary_product_gets_separable_tile
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 4 : i32 iter_args
// CHECK: = d_affine.for %{{[0-9]+}} = #map(%[[TILE]]) to #map
// CHECK-NOT: d_affine.if
// CHECK-NOT: arith.minsi

// CHECK-LABEL: func.func @shape_product_prefers_exact
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 4 : i32 iter_args
// CHECK: = d_affine.for %{{[0-9]+}} = #map(%[[TILE]]) to #map
// CHECK-NOT: d_affine.if
// CHECK-NOT: arith.minsi

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

  func.func @size_product_still_prefers_exact(%init: f32) -> f32 {
    %n0 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
    %tile_size = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
    %n_size = "d_tensor.size.mul"(%n0, %tile_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n_size) step 1 : index iter_args(%acc = %init : f32) {
      d_affine.yield %acc : (f32)
    }
    func.return %sum : f32
  }
}

// CHECK-LABEL: func.func @ordinary_product_gets_separable_tile
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 4 : i32 iter_args
// CHECK: "d_affine.if"(%[[TILE]], %{{[0-9]+}}, %{{[0-9]+}}) <{condition = #set}> ({
// The generated guard uses operands as (tile iv dim, tile size dim, full upper bound symbol).
// CHECK: d_affine.yield
// CHECK: }, {
// CHECK: d_affine.min
// CHECK: d_affine.yield
// CHECK: }) : (index, index, index) -> f32

// CHECK-LABEL: func.func @size_product_still_prefers_exact
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 4 : i32 iter_args
// CHECK: = d_affine.for %{{[0-9]+}} = #map(%[[TILE]]) to #map
// CHECK-NOT: d_affine.if
// CHECK-NOT: arith.minsi

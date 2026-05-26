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

  func.func @natmul_still_prefers_exact(%init: f32) -> f32 {
    %n0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %tile_nat = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %n_nat = "dtensor.nat.mul"(%n0, %tile_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %n = "dtensor.shape.to_index"(%n_nat) : (!dtensor.nat) -> index
    %sum = d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%n) step 1 : index iter_args(%acc = %init : f32) {
      d_affine.yield %acc : (f32)
    }
    func.return %sum : f32
  }
}

// CHECK-LABEL: func.func @ordinary_product_gets_separable_tile
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 4 : i32 iter_args
// CHECK: "d_affine.if"(%[[TILE]], %{{[0-9]+}}, %{{[0-9]+}}) <{condition = #set}> ({
// CHECK: d_affine.yield
// CHECK: }, {
// CHECK: arith.minsi
// CHECK: d_affine.yield
// CHECK: }) : (index, index, index) -> f32

// CHECK-LABEL: func.func @natmul_still_prefers_exact
// CHECK: d_affine.for %[[TILE:[0-9]+]] = #map(%{{[0-9]+}}) to #map(%{{[0-9]+}}) step 4 : i32 iter_args
// CHECK: = d_affine.for %{{[0-9]+}} = #map(%[[TILE]]) to #map
// CHECK-NOT: d_affine.if
// CHECK-NOT: arith.minsi

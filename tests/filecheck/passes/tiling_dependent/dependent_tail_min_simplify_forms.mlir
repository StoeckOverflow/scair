// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min

builtin.module {
  func.func @affine_min_form() {
    %k0 = "test.index"() : () -> index
    %k1 = "arith.constant"() <{value = 8 : index}> : () -> index
    %full = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %step = "arith.addi"(%k1, %c0) : (index, index) -> index
    %tile_size = "arith.addi"(%k1, %c0) : (index, index) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
      %clamped = "affine.min"(%tile, %tile_size, %full) <{map = affine_map<(d0, d1, d2) -> (d0 + d1, d2)>}> : (index, index, index) -> index
      %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
        %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc1)[] : (index, index)[] -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    "test.keep"(%sum) : (index) -> ()
    "func.return"() : () -> ()
  }

  func.func @d_affine_min_form() {
    %k0 = "test.index"() : () -> index
    %k1 = "arith.constant"() <{value = 8 : index}> : () -> index
    %full = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %step = "arith.addi"(%k1, %c0) : (index, index) -> index
    %tile_size = "arith.addi"(%k1, %c0) : (index, index) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
      %clamped = d_affine.min affine_map<(d0)[s0, s1] -> (d0 + s0, s1)>(%tile)[%tile_size, %full] : (index)[index, index] -> index
      %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
        %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc1)[] : (index, index)[] -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    "test.keep"(%sum) : (index) -> ()
    "func.return"() : () -> ()
  }

  func.func @d_affine_apply_tile_end_form() {
    %k0 = "test.index"() : () -> index
    %k1 = "arith.constant"() <{value = 8 : index}> : () -> index
    %full = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %step = "arith.addi"(%k1, %c0) : (index, index) -> index
    %tile_size = "arith.addi"(%k1, %c0) : (index, index) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
      %tile_end = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%tile, %tile_size)[] : (index, index)[] -> index
      %clamped = "arith.minsi"(%tile_end, %full) : (index, index) -> index
      %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
        %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc1)[] : (index, index)[] -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    "test.keep"(%sum) : (index) -> ()
    "func.return"() : () -> ()
  }

  func.func @product_factor_form() {
    %k0 = "arith.constant"() <{value = 4 : index}> : () -> index
    %k1 = "arith.constant"() <{value = 8 : index}> : () -> index
    %k2 = "test.index"() : () -> index
    %tile_size = "arith.muli"(%k1, %k0) : (index, index) -> index
    %full = "arith.muli"(%tile_size, %k2) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %tile_size : index iter_args(%acc0 = %init : index) {
      %tile_end = "arith.addi"(%tile, %tile_size) : (index, index) -> index
      %clamped = "arith.minsi"(%tile_end, %full) : (index, index) -> index
      %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
        %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc1)[] : (index, index)[] -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    "test.keep"(%sum) : (index) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @affine_min_form
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] =
// CHECK: %[[TILE_END:[0-9]+]] = "affine.apply"(%[[TILE]], %{{[0-9]+}}, %{{[0-9]+}})
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]], %{{[0-9]+}})[]

// CHECK-LABEL: func.func @d_affine_min_form
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] =
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%{{[0-9]+}}, %{{[0-9]+}}]
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]], %{{[0-9]+}})[]

// CHECK-LABEL: func.func @d_affine_apply_tile_end_form
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] =
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]], %{{[0-9]+}})[]
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]], %{{[0-9]+}})[]

// CHECK-LABEL: func.func @product_factor_form
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] =
// CHECK: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %{{[0-9]+}})
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]], %{{[0-9]+}})[]

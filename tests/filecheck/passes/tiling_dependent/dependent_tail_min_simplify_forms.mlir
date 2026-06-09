// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min

builtin.module {
  func.func @affine_min_form() {
    %k0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
    %k1 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
    %k = "d_tensor.nat.mul"(%k0, %k1) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %full = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
    %step = "d_tensor.shape.to_index"(%k1) : (!d_tensor.posnat) -> index
    %tile_size = "d_tensor.shape.to_index"(%k1) : (!d_tensor.posnat) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
      %clamped = "affine.min"(%tile, %tile_size, %full) <{map = affine_map<(d0, d1, d2) -> (d0 + d1, d2)>}> : (index, index, index) -> index
      %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
        %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc1] : (index)[index] -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    "test.keep"(%sum) : (index) -> ()
    "func.return"() : () -> ()
  }

  func.func @d_affine_min_form() {
    %k0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
    %k1 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
    %k = "d_tensor.nat.mul"(%k0, %k1) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %full = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
    %step = "d_tensor.shape.to_index"(%k1) : (!d_tensor.posnat) -> index
    %tile_size = "d_tensor.shape.to_index"(%k1) : (!d_tensor.posnat) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
      %clamped = d_affine.min affine_map<(d0)[s0, s1] -> (d0 + s0, s1)>(%tile)[%tile_size, %full] : (index)[index, index] -> index
      %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
        %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc1] : (index)[index] -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    "test.keep"(%sum) : (index) -> ()
    "func.return"() : () -> ()
  }

  func.func @d_affine_apply_tile_end_form() {
    %k0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
    %k1 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
    %k = "d_tensor.nat.mul"(%k0, %k1) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %full = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
    %step = "d_tensor.shape.to_index"(%k1) : (!d_tensor.posnat) -> index
    %tile_size = "d_tensor.shape.to_index"(%k1) : (!d_tensor.posnat) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
      %tile_end = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%tile)[%tile_size] : (index)[index] -> index
      %clamped = "arith.minsi"(%tile_end, %full) : (index, index) -> index
      %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
        %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc1] : (index)[index] -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    "test.keep"(%sum) : (index) -> ()
    "func.return"() : () -> ()
  }

  func.func @product_factor_form() {
    %k0 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
    %k1 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
    %k2 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
    %tile_nat = "d_tensor.nat.mul"(%k1, %k0) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
    %full_nat = "d_tensor.nat.mul"(%tile_nat, %k2) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %full = "d_tensor.shape.to_index"(%full_nat) : (!d_tensor.posnat) -> index
    %step = "d_tensor.shape.to_index"(%tile_nat) : (!d_tensor.posnat) -> index
    %tile_size = "d_tensor.shape.to_index"(%tile_nat) : (!d_tensor.posnat) -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
      %tile_end = "arith.addi"(%tile, %tile_size) : (index, index) -> index
      %clamped = "arith.minsi"(%tile_end, %full) : (index, index) -> index
      %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
        %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc1] : (index)[index] -> index
        d_affine.yield %next : (index)
      }
      d_affine.yield %inner : (index)
    }
    "test.keep"(%sum) : (index) -> ()
    "func.return"() : () -> ()
  }
}

// CHECK-LABEL: func.func @affine_min_form
// CHECK: %[[K0:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK: %[[K1:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.posnat
// CHECK: %[[K:[0-9]+]] = "d_tensor.nat.mul"(%[[K0]], %[[K1]]) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
// CHECK: %[[FULL:[0-9]+]] = "d_tensor.shape.to_index"(%[[K]]) : (!d_tensor.nat) -> index
// CHECK: %[[STEP:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1]]) : (!d_tensor.posnat) -> index
// CHECK: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1]]) : (!d_tensor.posnat) -> index
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0:[0-9]+]]) to #map(%[[FULL]]) step %[[STEP]] : index iter_args(%[[ACC0:[0-9]+]] = %[[INIT:[0-9]+]] : index) {
// CHECK: %[[TILE_END:[0-9]+]] = "affine.apply"(%[[TILE]], %[[TILE_SIZE]], %[[FULL]]) <{map = #map{{[0-9]*}}}> : (index, index, index) -> index
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]])[%[[ACC1]]] : (index)[index] -> index

// CHECK-LABEL: func.func @d_affine_min_form
// CHECK: %[[K0:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK: %[[K1:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.posnat
// CHECK: %[[K:[0-9]+]] = "d_tensor.nat.mul"(%[[K0]], %[[K1]]) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
// CHECK: %[[FULL:[0-9]+]] = "d_tensor.shape.to_index"(%[[K]]) : (!d_tensor.nat) -> index
// CHECK: %[[STEP:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1]]) : (!d_tensor.posnat) -> index
// CHECK: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1]]) : (!d_tensor.posnat) -> index
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0:[0-9]+]]) to #map(%[[FULL]]) step %[[STEP]] : index iter_args(%[[ACC0:[0-9]+]] = %[[INIT:[0-9]+]] : index) {
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[TILE_SIZE]], %[[FULL]]] : (index)[index, index] -> index
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]])[%[[ACC1]]] : (index)[index] -> index

// CHECK-LABEL: func.func @d_affine_apply_tile_end_form
// CHECK: %[[K0:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK: %[[K1:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.posnat
// CHECK: %[[K:[0-9]+]] = "d_tensor.nat.mul"(%[[K0]], %[[K1]]) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
// CHECK: %[[FULL:[0-9]+]] = "d_tensor.shape.to_index"(%[[K]]) : (!d_tensor.nat) -> index
// CHECK: %[[STEP:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1]]) : (!d_tensor.posnat) -> index
// CHECK: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[K1]]) : (!d_tensor.posnat) -> index
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0:[0-9]+]]) to #map(%[[FULL]]) step %[[STEP]] : index iter_args(%[[ACC0:[0-9]+]] = %[[INIT:[0-9]+]] : index) {
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[TILE_SIZE]]] : (index)[index] -> index
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]])[%[[ACC1]]] : (index)[index] -> index

// CHECK-LABEL: func.func @product_factor_form
// CHECK: %[[K0:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.posnat
// CHECK: %[[K1:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.posnat
// CHECK: %[[K2:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.posnat
// CHECK: %[[TILE_NAT:[0-9]+]] = "d_tensor.nat.mul"(%[[K1]], %[[K0]]) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
// CHECK: %[[FULL_NAT:[0-9]+]] = "d_tensor.nat.mul"(%[[TILE_NAT]], %[[K2]]) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
// CHECK: %[[FULL:[0-9]+]] = "d_tensor.shape.to_index"(%[[FULL_NAT]]) : (!d_tensor.posnat) -> index
// CHECK: %[[STEP:[0-9]+]] = "d_tensor.shape.to_index"(%[[TILE_NAT]]) : (!d_tensor.posnat) -> index
// CHECK: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[TILE_NAT]]) : (!d_tensor.posnat) -> index
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0:[0-9]+]]) to #map(%[[FULL]]) step %[[STEP]] : index iter_args(%[[ACC0:[0-9]+]] = %[[INIT:[0-9]+]] : index) {
// CHECK: %[[TILE_END:[0-9]+]] = "arith.addi"(%[[TILE]], %[[TILE_SIZE]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]])[%[[ACC1]]] : (index)[index] -> index

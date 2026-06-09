// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min

builtin.module {
  func.func @affine_min_form() {
    %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
    %k1 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
    %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step %k1 : index iter_args(%acc0 = %init : index) {
      %clamped = d_affine.min affine_map<(d0)[s0, s1] -> (d0 + s0, s1)>(%tile)[%k1, %k] : (index)[!d_tensor.pos_size, !d_tensor.size] -> index
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
    %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
    %k1 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
    %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step %k1 : index iter_args(%acc0 = %init : index) {
      %clamped = d_affine.min affine_map<(d0)[s0, s1] -> (d0 + s0, s1)>(%tile)[%k1, %k] : (index)[!d_tensor.pos_size, !d_tensor.size] -> index
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
    %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
    %k1 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
    %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step %k1 : index iter_args(%acc0 = %init : index) {
      %tile_end = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%tile)[%k1] : (index)[!d_tensor.pos_size] -> index
      %clamped = d_affine.min affine_map<(d0)[s0] -> (d0, s0)>(%tile_end)[%k] : (index)[!d_tensor.size] -> index
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
    %k0 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
    %k1 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
    %k2 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
    %tile_size = "d_tensor.size.mul"(%k1, %k0) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
    %full_size = "d_tensor.size.mul"(%tile_size, %k2) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full_size) step %tile_size : index iter_args(%acc0 = %init : index) {
      %tile_end = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%tile)[%tile_size] : (index)[!d_tensor.pos_size] -> index
      %clamped = d_affine.min affine_map<(d0)[s0] -> (d0, s0)>(%tile_end)[%full_size] : (index)[!d_tensor.pos_size] -> index
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
// CHECK: %[[K0:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK: %[[K1:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// CHECK: %[[K:[0-9]+]] = "d_tensor.size.mul"(%[[K0]], %[[K1]]) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0:[0-9]+]]) to #map(%[[K]]) step %[[K1]] : !d_tensor.pos_size iter_args(%[[ACC0:[0-9]+]] = %[[INIT:[0-9]+]] : index) {
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[K1]], %[[K]]] : (index)[!d_tensor.pos_size, !d_tensor.size] -> index
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]])[%[[ACC1]]] : (index)[index] -> index

// CHECK-LABEL: func.func @d_affine_min_form
// CHECK: %[[K0:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK: %[[K1:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// CHECK: %[[K:[0-9]+]] = "d_tensor.size.mul"(%[[K0]], %[[K1]]) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0:[0-9]+]]) to #map(%[[K]]) step %[[K1]] : !d_tensor.pos_size iter_args(%[[ACC0:[0-9]+]] = %[[INIT:[0-9]+]] : index) {
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[K1]], %[[K]]] : (index)[!d_tensor.pos_size, !d_tensor.size] -> index
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[TILE_END]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]])[%[[ACC1]]] : (index)[index] -> index

// CHECK-LABEL: func.func @d_affine_apply_tile_end_form
// CHECK: %[[K0:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK: %[[K1:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// CHECK: %[[K:[0-9]+]] = "d_tensor.size.mul"(%[[K0]], %[[K1]]) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0:[0-9]+]]) to #map(%[[K]]) step %[[K1]] : !d_tensor.pos_size iter_args(%[[ACC0:[0-9]+]] = %[[INIT:[0-9]+]] : index) {
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[K1]]] : (index)[!d_tensor.pos_size] -> index
// CHECK: %[[CLAMPED:[0-9]+]] = d_affine.min #map{{[0-9]*}} (%[[TILE_END]])[%[[K]]] : (index)[!d_tensor.size] -> index
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]])[%[[ACC1]]] : (index)[index] -> index

// CHECK-LABEL: func.func @product_factor_form
// CHECK: %[[K0:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// CHECK: %[[K1:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// CHECK: %[[K2:[0-9]+]] = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// CHECK: %[[TILE_NAT:[0-9]+]] = "d_tensor.size.mul"(%[[K1]], %[[K0]]) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
// CHECK: %[[FULL_NAT:[0-9]+]] = "d_tensor.size.mul"(%[[TILE_NAT]], %[[K2]]) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
// CHECK: %[[SUM:[0-9]+]] = d_affine.for %[[TILE:[0-9]+]] = #map(%[[C0:[0-9]+]]) to #map(%[[FULL_NAT]]) step %[[TILE_NAT]] : !d_tensor.pos_size iter_args(%[[ACC0:[0-9]+]] = %[[INIT:[0-9]+]] : index) {
// CHECK: %[[TILE_END:[0-9]+]] = d_affine.apply #map{{[0-9]*}} (%[[TILE]])[%[[TILE_NAT]]] : (index)[!d_tensor.pos_size] -> index
// CHECK: %[[CLAMPED:[0-9]+]] = d_affine.min #map{{[0-9]*}} (%[[TILE_END]])[%[[FULL_NAT]]] : (index)[!d_tensor.pos_size] -> index
// CHECK: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map(%[[CLAMPED]]) step 1 : index iter_args(%[[ACC1:[0-9]+]] = %[[ACC0]] : index) {
// CHECK: d_affine.apply #map{{[0-9]*}} (%[[P]])[%[[ACC1]]] : (index)[index] -> index

// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile | filecheck %s --check-prefix=RIGHT --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile:leftmost-positive | filecheck %s --check-prefix=LEFT --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile:factor-index=1 | filecheck %s --check-prefix=INDEX1 --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,dependent-product-loop-exact-tile | filecheck %s --check-prefix=TWICE --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s --check-prefix=WRONG-TAIL

builtin.module {
  func.func @policy_product(%out: memref<?xf32>) {
    %k0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %k1 = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
    %k = "dtensor.nat.mul"(%k0, %k1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %ub = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    "func.return"() : () -> ()
  }

  func.func @triple_index_product(%out: memref<?xf32>) {
    %a = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
    %b = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
    %c = "dtensor.nat.const"() <{value = 11 : i32}> : () -> !dtensor.nat
    %ab = "dtensor.nat.mul"(%a, %b) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %abc = "dtensor.nat.mul"(%ab, %c) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %ub = "dtensor.shape.to_index"(%abc) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    "func.return"() : () -> ()
  }

  func.func @non_zero_lower_product_is_unchanged(%out: memref<?xf32>) {
    %k0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %k1 = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
    %k = "dtensor.nat.mul"(%k0, %k1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %ub = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %p = affine_map<(d0) -> (d0)>(%c1) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    "func.return"() : () -> ()
  }

  func.func @wrong_factor_tail_guard_is_preserved(%out: memref<?xf32>) {
    %k0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %k1 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %k = "dtensor.nat.mul"(%k0, %k1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %ub = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
    %k0_idx = "dtensor.shape.to_index"(%k0) : (!dtensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 8 : index {
      %end = "arith.addi"(%tile, %k0_idx) : (index, index) -> index
      %clamped = "arith.minsi"(%end, %ub) : (index, index) -> index
      d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index {
        "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
        d_affine.yield
      }
      d_affine.yield
    }
    "func.return"() : () -> ()
  }
}

// RIGHT-LABEL: func.func @policy_product
// RIGHT: %[[K1:[0-9]+]] = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
// RIGHT: %[[TILE:[0-9]+]] = "dtensor.shape.to_index"(%[[K1]]) : (!dtensor.nat) -> index
// RIGHT: d_affine.for %{{[0-9]+}} = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 5

// LEFT-LABEL: func.func @policy_product
// LEFT: %[[K0:[0-9]+]] = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
// LEFT: %[[TILE:[0-9]+]] = "dtensor.shape.to_index"(%[[K0]]) : (!dtensor.nat) -> index
// LEFT: d_affine.for %{{[0-9]+}} = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 3

// INDEX1-LABEL: func.func @triple_index_product
// INDEX1: %[[B:[0-9]+]] = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
// INDEX1: %[[TILE:[0-9]+]] = "dtensor.shape.to_index"(%[[B]]) : (!dtensor.nat) -> index
// INDEX1: d_affine.for %{{[0-9]+}} = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 7

// TWICE-LABEL: func.func @policy_product
// TWICE: d_affine.for %[[OUTER:[0-9]+]] = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 5
// TWICE-NEXT: d_affine.for %[[INNER:[0-9]+]] = #map{{[0-9]*}}(%[[OUTER]]) to #map{{[0-9]*}}(%[[OUTER]]) step 1
// TWICE: "memref.store"(%{{[0-9]+}}, %{{[0-9]+}}, %[[INNER]])
// TWICE-LABEL: func.func @non_zero_lower_product_is_unchanged
// TWICE: d_affine.for %[[P:[0-9]+]] = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 1
// TWICE: "memref.store"(%{{[0-9]+}}, %{{[0-9]+}}, %[[P]])

// WRONG-TAIL-LABEL: func.func @wrong_factor_tail_guard_is_preserved
// WRONG-TAIL: "arith.minsi"

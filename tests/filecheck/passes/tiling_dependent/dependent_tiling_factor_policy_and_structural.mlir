// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile | filecheck %s --check-prefix=RIGHT --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile:leftmost-positive | filecheck %s --check-prefix=LEFT --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile:factor-index=1 | filecheck %s --check-prefix=INDEX1 --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,dependent-product-loop-exact-tile | filecheck %s --check-prefix=TWICE --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s --check-prefix=WRONG-TAIL

builtin.module {
  func.func @policy_product(%out: memref<?xf32>) {
    %k0 = "arith.constant"() <{value = 3 : index}> : () -> index
    %k1 = "arith.constant"() <{value = 5 : index}> : () -> index
    %ub = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    "func.return"() : () -> ()
  }

  func.func @triple_index_product(%out: memref<?xf32>) {
    %a = "arith.constant"() <{value = 2 : index}> : () -> index
    %b = "arith.constant"() <{value = 7 : index}> : () -> index
    %c = "arith.constant"() <{value = 11 : index}> : () -> index
    %ab = "arith.muli"(%a, %b) : (index, index) -> index
    %ub = "arith.muli"(%ab, %c) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    "func.return"() : () -> ()
  }

  func.func @non_zero_lower_product_is_unchanged(%out: memref<?xf32>) {
    %k0 = "arith.constant"() <{value = 3 : index}> : () -> index
    %k1 = "arith.constant"() <{value = 5 : index}> : () -> index
    %ub = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %p = affine_map<(d0) -> (d0)>(%c1) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    "func.return"() : () -> ()
  }

  func.func @wrong_factor_tail_guard_is_preserved(%out: memref<?xf32>) {
    %k0_idx = "arith.constant"() <{value = 4 : index}> : () -> index
    %k1 = "arith.constant"() <{value = 8 : index}> : () -> index
    %ub = "arith.muli"(%k0_idx, %k1) : (index, index) -> index
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
// RIGHT: %[[K1:[0-9]+]] = "arith.constant"() <{value = 5 : index}> : () -> index
// RIGHT: d_affine.for %{{[0-9]+}} = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 5

// LEFT-LABEL: func.func @policy_product
// LEFT: %[[K0:[0-9]+]] = "arith.constant"() <{value = 3 : index}> : () -> index
// LEFT: d_affine.for %{{[0-9]+}} = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 3

// INDEX1-LABEL: func.func @triple_index_product
// INDEX1: %[[B:[0-9]+]] = "arith.constant"() <{value = 7 : index}> : () -> index
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

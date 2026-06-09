// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile | filecheck %s --check-prefix=RIGHT --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile:leftmost-positive | filecheck %s --check-prefix=LEFT --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile:factor-index=1 | filecheck %s --check-prefix=INDEX1 --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,dependent-product-loop-exact-tile | filecheck %s --check-prefix=TWICE --implicit-check-not=scair.dependent --implicit-check-not=scair.ordinary
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-tail-min-simplify | filecheck %s --check-prefix=WRONG-TAIL

builtin.module {
  func.func @policy_product(%out: memref<?xf32>) {
    %k0 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
    %k1 = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
    %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    "func.return"() : () -> ()
  }

  func.func @triple_index_product(%out: memref<?xf32>) {
    %a = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
    %b = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
    %c = "d_tensor.size.constant"() <{value = 11 : i32}> : () -> !d_tensor.size
    %ab = "d_tensor.size.mul"(%a, %b) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %abc = "d_tensor.size.mul"(%ab, %c) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%abc) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    "func.return"() : () -> ()
  }

  func.func @non_zero_lower_product_is_unchanged(%out: memref<?xf32>) {
    %k0 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
    %k1 = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
    %abc = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %p = affine_map<(d0) -> (d0)>(%c1) to affine_map<(d0) -> (d0)>(%abc) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }
    "func.return"() : () -> ()
  }

  func.func @wrong_factor_tail_guard_is_preserved(%out: memref<?xf32>) {
    %k0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
    %k1 = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
    %abc = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%abc) step 8 : index {
      %end = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%tile)[%k0] : (index)[!d_tensor.size] -> index
      %clamped = d_affine.min affine_map<(d0)[s0] -> (d0, s0)>(%end)[%abc] : (index)[!d_tensor.size] -> index
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
// RIGHT: %[[K1:[0-9]+]] = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
// RIGHT: d_affine.for %{{[0-9]+}} = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 5

// LEFT-LABEL: func.func @policy_product
// LEFT: %[[K0:[0-9]+]] = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// LEFT: d_affine.for %{{[0-9]+}} = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 3

// INDEX1-LABEL: func.func @triple_index_product
// INDEX1: %[[B:[0-9]+]] = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
// INDEX1: d_affine.for %{{[0-9]+}} = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 7

// TWICE-LABEL: func.func @policy_product
// TWICE: d_affine.for %[[OUTER:[0-9]+]] = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 5
// TWICE-NEXT: d_affine.for %[[INNER:[0-9]+]] = #map{{[0-9]*}}(%[[OUTER]]) to #map{{[0-9]*}}(%[[OUTER]]) step 1
// TWICE: "memref.store"(%{{[0-9]+}}, %{{[0-9]+}}, %[[INNER]])
// TWICE-LABEL: func.func @non_zero_lower_product_is_unchanged
// TWICE: d_affine.for %[[P:[0-9]+]] = #map{{[0-9]*}}(%{{[0-9]+}}) to #map{{[0-9]*}}(%{{[0-9]+}}) step 1
// TWICE: "memref.store"(%{{[0-9]+}}, %{{[0-9]+}}, %[[P]])

// WRONG-TAIL-LABEL: func.func @wrong_factor_tail_guard_is_preserved
// WRONG-TAIL: d_affine.min

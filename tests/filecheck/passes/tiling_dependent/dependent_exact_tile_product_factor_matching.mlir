// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile | filecheck %s --check-prefix=LAZY --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min
// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-nat-products,dependent-product-loop-exact-tile | filecheck %s --check-prefix=EAGER --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min

builtin.module {
  func.func @commuted_static_product(%out: memref<?xf32>) {
    %k1_nat = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
    %k0_nat = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    %k_nat = "d_tensor.nat.mul"(%k1_nat, %k0_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %ub = "d_tensor.shape.to_index"(%k_nat) : (!d_tensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }

  func.func @nested_static_product(%out: memref<?xf32>) {
    %k0_nat = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
    %k1_nat = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
    %k2_nat = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
    %k01_nat = "d_tensor.nat.mul"(%k0_nat, %k1_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %k_nat = "d_tensor.nat.mul"(%k01_nat, %k2_nat) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %ub = "d_tensor.shape.to_index"(%k_nat) : (!d_tensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// LAZY: #map = affine_map<(d0)[] -> (d0)>
// LAZY: #map1 = affine_map<(d0)[] -> (d0 + 4)>
// LAZY: #map2 = affine_map<(d0)[] -> (d0 + 7)>
// LAZY-LABEL: func.func @commuted_static_product
// LAZY-SAME: %[[OUT:[0-9]+]]: memref<?xf32>
// LAZY: %[[K8:[0-9]+]] = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
// LAZY: %[[K4:[0-9]+]] = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// LAZY: %[[K:[0-9]+]] = "d_tensor.nat.mul"(%[[K8]], %[[K4]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// LAZY: %[[UB:[0-9]+]] = "d_tensor.shape.to_index"(%[[K]]) : (!d_tensor.nat) -> index
// LAZY: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// LAZY: %[[CST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// LAZY: %[[TILE_C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// LAZY: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[K4]]) : (!d_tensor.nat) -> index
// LAZY: d_affine.for %[[TILE:[0-9]+]] = #map(%[[TILE_C0]]) to #map(%[[UB]]) step 4 : i32 {
// LAZY: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map1(%[[TILE]]) step 1 : i32 {
// LAZY: "memref.store"(%[[CST]], %[[OUT]], %[[P]]) : (f32, memref<?xf32>, index) -> ()
// LAZY-LABEL: func.func @nested_static_product
// LAZY-SAME: %[[NESTED_OUT:[0-9]+]]: memref<?xf32>
// LAZY: %[[N0:[0-9]+]] = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
// LAZY: %[[N1:[0-9]+]] = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
// LAZY: %[[N2:[0-9]+]] = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
// LAZY: %[[N01:[0-9]+]] = "d_tensor.nat.mul"(%[[N0]], %[[N1]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// LAZY: %[[NK:[0-9]+]] = "d_tensor.nat.mul"(%[[N01]], %[[N2]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// LAZY: %[[NUB:[0-9]+]] = "d_tensor.shape.to_index"(%[[NK]]) : (!d_tensor.nat) -> index
// LAZY: %[[NC0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// LAZY: %[[NCST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// LAZY: %[[NTILE_C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// LAZY: %[[NTILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[N2]]) : (!d_tensor.nat) -> index
// LAZY: d_affine.for %[[NTILE:[0-9]+]] = #map(%[[NTILE_C0]]) to #map(%[[NUB]]) step 7 : i32 {
// LAZY: d_affine.for %[[NP:[0-9]+]] = #map(%[[NTILE]]) to #map2(%[[NTILE]]) step 1 : i32 {
// LAZY: "memref.store"(%[[NCST]], %[[NESTED_OUT]], %[[NP]]) : (f32, memref<?xf32>, index) -> ()

// EAGER: #map = affine_map<(d0)[] -> (d0)>
// EAGER: #map1 = affine_map<(d0)[] -> (d0 + 8)>
// EAGER: #map2 = affine_map<(d0)[] -> (d0 + 7)>
// EAGER-LABEL: func.func @commuted_static_product
// EAGER-SAME: %[[OUT:[0-9]+]]: memref<?xf32>
// EAGER: %[[K8:[0-9]+]] = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
// EAGER: %[[K4:[0-9]+]] = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// EAGER: %[[K:[0-9]+]] = "d_tensor.nat.mul"(%[[K4]], %[[K8]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// EAGER: %[[UB:[0-9]+]] = "d_tensor.shape.to_index"(%[[K]]) : (!d_tensor.nat) -> index
// EAGER: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EAGER: %[[CST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// EAGER: %[[TILE_C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EAGER: %[[TILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[K8]]) : (!d_tensor.nat) -> index
// EAGER: d_affine.for %[[TILE:[0-9]+]] = #map(%[[TILE_C0]]) to #map(%[[UB]]) step 8 : i32 {
// EAGER: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map1(%[[TILE]]) step 1 : i32 {
// EAGER: "memref.store"(%[[CST]], %[[OUT]], %[[P]]) : (f32, memref<?xf32>, index) -> ()
// EAGER-LABEL: func.func @nested_static_product
// EAGER-SAME: %[[NESTED_OUT:[0-9]+]]: memref<?xf32>
// EAGER: %[[N0:[0-9]+]] = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
// EAGER: %[[N1:[0-9]+]] = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
// EAGER: %[[N2:[0-9]+]] = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
// EAGER: %[[N01:[0-9]+]] = "d_tensor.nat.mul"(%[[N0]], %[[N1]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// EAGER: %[[NK:[0-9]+]] = "d_tensor.nat.mul"(%[[N01]], %[[N2]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// EAGER: %[[NUB:[0-9]+]] = "d_tensor.shape.to_index"(%[[NK]]) : (!d_tensor.nat) -> index
// EAGER: %[[NC0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EAGER: %[[NCST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// EAGER: %[[NTILE_C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EAGER: %[[NTILE_SIZE:[0-9]+]] = "d_tensor.shape.to_index"(%[[N2]]) : (!d_tensor.nat) -> index
// EAGER: d_affine.for %[[NTILE:[0-9]+]] = #map(%[[NTILE_C0]]) to #map(%[[NUB]]) step 7 : i32 {
// EAGER: d_affine.for %[[NP:[0-9]+]] = #map(%[[NTILE]]) to #map2(%[[NTILE]]) step 1 : i32 {
// EAGER: "memref.store"(%[[NCST]], %[[NESTED_OUT]], %[[NP]]) : (f32, memref<?xf32>, index) -> ()

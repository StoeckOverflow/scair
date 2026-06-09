// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile | filecheck %s --check-prefix=LAZY --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min
// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-d-tensor-size-products,dependent-product-loop-exact-tile | filecheck %s --check-prefix=EAGER --implicit-check-not=arith.minsi --implicit-check-not=affine.min --implicit-check-not=d_affine.min

builtin.module {
  func.func @commuted_static_product(%out: memref<?xf32>) {
    %k1_size = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
    %k0_size = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
    %k_size = "d_tensor.size.mul"(%k1_size, %k0_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k_size) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }

  func.func @nested_static_product(%out: memref<?xf32>) {
    %k0_size = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
    %k1_size = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
    %k2_size = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
    %k01_size = "d_tensor.size.mul"(%k0_size, %k1_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %k_size = "d_tensor.size.mul"(%k01_size, %k2_size) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k_size) step 1 : index {
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
// LAZY: %[[K8:[0-9]+]] = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
// LAZY: %[[K4:[0-9]+]] = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// LAZY: %[[K:[0-9]+]] = "d_tensor.size.mul"(%[[K8]], %[[K4]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// LAZY: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// LAZY: %[[CST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// LAZY: %[[TILE_C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// LAZY: d_affine.for %[[TILE:[0-9]+]] = #map(%[[TILE_C0]]) to #map(%[[K]]) step 4 : i32 {
// LAZY: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map1(%[[TILE]]) step 1 : i32 {
// LAZY: "memref.store"(%[[CST]], %[[OUT]], %[[P]]) : (f32, memref<?xf32>, index) -> ()
// LAZY-LABEL: func.func @nested_static_product
// LAZY-SAME: %[[NESTED_OUT:[0-9]+]]: memref<?xf32>
// LAZY: %[[N0:[0-9]+]] = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// LAZY: %[[N1:[0-9]+]] = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
// LAZY: %[[N2:[0-9]+]] = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
// LAZY: %[[N01:[0-9]+]] = "d_tensor.size.mul"(%[[N0]], %[[N1]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// LAZY: %[[NK:[0-9]+]] = "d_tensor.size.mul"(%[[N01]], %[[N2]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// LAZY: %[[NC0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// LAZY: %[[NCST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// LAZY: %[[NTILE_C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// LAZY: d_affine.for %[[NTILE:[0-9]+]] = #map(%[[NTILE_C0]]) to #map(%[[NK]]) step 7 : i32 {
// LAZY: d_affine.for %[[NP:[0-9]+]] = #map(%[[NTILE]]) to #map2(%[[NTILE]]) step 1 : i32 {
// LAZY: "memref.store"(%[[NCST]], %[[NESTED_OUT]], %[[NP]]) : (f32, memref<?xf32>, index) -> ()

// EAGER: #map = affine_map<(d0)[] -> (d0)>
// EAGER: #map1 = affine_map<(d0)[] -> (d0 + 8)>
// EAGER: #map2 = affine_map<(d0)[] -> (d0 + 7)>
// EAGER-LABEL: func.func @commuted_static_product
// EAGER-SAME: %[[OUT:[0-9]+]]: memref<?xf32>
// EAGER: %[[K8:[0-9]+]] = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
// EAGER: %[[K4:[0-9]+]] = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// EAGER: %[[K:[0-9]+]] = "d_tensor.size.mul"(%[[K4]], %[[K8]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// EAGER: %[[C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EAGER: %[[CST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// EAGER: %[[TILE_C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EAGER: d_affine.for %[[TILE:[0-9]+]] = #map(%[[TILE_C0]]) to #map(%[[K]]) step 8 : i32 {
// EAGER: d_affine.for %[[P:[0-9]+]] = #map(%[[TILE]]) to #map1(%[[TILE]]) step 1 : i32 {
// EAGER: "memref.store"(%[[CST]], %[[OUT]], %[[P]]) : (f32, memref<?xf32>, index) -> ()
// EAGER-LABEL: func.func @nested_static_product
// EAGER-SAME: %[[NESTED_OUT:[0-9]+]]: memref<?xf32>
// EAGER: %[[N0:[0-9]+]] = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// EAGER: %[[N1:[0-9]+]] = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
// EAGER: %[[N2:[0-9]+]] = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
// EAGER: %[[N01:[0-9]+]] = "d_tensor.size.mul"(%[[N0]], %[[N1]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// EAGER: %[[NK:[0-9]+]] = "d_tensor.size.mul"(%[[N01]], %[[N2]]) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// EAGER: %[[NC0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EAGER: %[[NCST:[0-9]+]] = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// EAGER: %[[NTILE_C0:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// EAGER: d_affine.for %[[NTILE:[0-9]+]] = #map(%[[NTILE_C0]]) to #map(%[[NK]]) step 7 : i32 {
// EAGER: d_affine.for %[[NP:[0-9]+]] = #map(%[[NTILE]]) to #map2(%[[NTILE]]) step 1 : i32 {
// EAGER: "memref.store"(%[[NCST]], %[[NESTED_OUT]], %[[NP]]) : (f32, memref<?xf32>, index) -> ()

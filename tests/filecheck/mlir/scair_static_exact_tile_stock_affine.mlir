// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,d-affine-to-affine-compatible | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)' | mlir-opt --allow-unregistered-dialect | filecheck %s --check-prefix=PARSE
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,d-affine-to-affine-compatible | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)' | mlir-opt --allow-unregistered-dialect --canonicalize | filecheck %s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,d-affine-to-affine-compatible | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)' | mlir-opt --allow-unregistered-dialect --affine-loop-normalize | filecheck %s --check-prefix=NORMALIZE
// RUN: scair-opt %s --allow-unregistered-dialect -p dependent-product-loop-exact-tile,d-affine-to-affine-compatible | grep -vE '^(NOTE: Picked up JDK_JAVA_OPTIONS:|Picked up _JAVA_OPTIONS:|\[[0-9.]+s\]\[warning\]\[perf,memops\] Cannot use file /tmp/hsperfdata_)' | mlir-opt --allow-unregistered-dialect --pass-pipeline='builtin.module(func.func(affine-loop-unroll{unroll-factor=8}))' | filecheck %s --check-prefix=UNROLL

builtin.module {
  func.func @static_exact_tile_stock_affine(%k0: index, %out: memref<?xf32>) {
    %k1 = "arith.constant"() <{value = 8 : index}> : () -> index
    %ub = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

// PARSE: affine_map<(d0) -> (d0 + 8)>
// PARSE-LABEL: func.func @static_exact_tile_stock_affine
// PARSE: affine.for %{{.*}} = {{.*}} step 8
// PARSE: affine.for %{{.*}} = {{.*}} to #map{{[0-9]+}}(%{{.*}})
// PARSE-NOT: d_affine.for
// PARSE-NOT: affine.min
// PARSE-NOT: arith.minsi

// CANON: affine_map<(d0) -> (d0 + 8)>
// CANON-LABEL: func.func @static_exact_tile_stock_affine
// CANON: affine.for %{{.*}} = {{.*}} step 8
// CANON-NOT: d_affine.for
// CANON-NOT: affine.min
// CANON-NOT: arith.minsi

// NORMALIZE-LABEL: func.func @static_exact_tile_stock_affine
// NORMALIZE: affine.for %{{.*}} = 0 to {{.*}}
// NORMALIZE: affine.for %{{.*}} = 0 to 8
// NORMALIZE-NOT: d_affine.for
// NORMALIZE-NOT: affine.min
// NORMALIZE-NOT: arith.minsi

// UNROLL-LABEL: func.func @static_exact_tile_stock_affine
// UNROLL: affine.for %{{.*}} = {{.*}} step 8
// UNROLL-NOT: affine.for %{{.*}} = {{.*}} step 1
// UNROLL-NOT: affine.min
// UNROLL-NOT: arith.minsi
// UNROLL-NOT: d_affine.for
// UNROLL: memref.store

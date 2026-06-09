// RUN: mlir-opt %S/../../../experiments/tiling_benchmarks/affine_tiling_benchmark/affine_runtime_product_mlir.mlir --affine-loop-tile=tile-size=3 | filecheck %s

// CHECK: %[[K:[A-Za-z0-9_]+]] = arith.muli %{{.*}}, %{{.*}} : index
// CHECK: affine.for %[[TILE:[A-Za-z0-9_]+]] = 0 to %[[K]] step 3
// CHECK: affine.for %{{.*}} = #map(%[[TILE]]) to min #map1(%[[TILE]])[%[[K]]]
// CHECK-NOT: d_tensor.nat.mul
// CHECK-NOT: d_affine.for

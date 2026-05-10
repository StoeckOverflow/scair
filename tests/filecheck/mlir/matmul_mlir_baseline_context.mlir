// RUN: mlir-opt %S/../../../experiments/matmul_tiling_benchmark/matmul_kernel_mlir_baseline.mlir --affine-loop-tile=tile-size=3 | filecheck %s

// CHECK: %[[K:[A-Za-z0-9_]+]] = arith.muli %{{.*}}, %{{.*}} : index
// CHECK: affine.for %{{.*}} = 0 to %{{.*}} step 3
// CHECK: affine.for %{{.*}} = 0 to %{{.*}} step 3
// CHECK-NOT: affine.for %{{.*}} = 0 to %[[K]] step 3
// CHECK: affine.for %{{.*}} = 0 to %[[K]] iter_args
// CHECK-NOT: dtensor.nat.mul
// CHECK-NOT: d_affine.for

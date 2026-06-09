// RUN: mlir-opt %S/../../../experiments/tiling_benchmarks/matmul_reduction_dim_tiling_benchmark/matmul_kernel_mlir_baseline.mlir --affine-loop-tile=tile-size=3 | filecheck %s

// CHECK: %[[K:[A-Za-z0-9_]+]] = arith.muli %{{.*}}, %{{.*}} : index
// CHECK: affine.for %{{.*}} = 0 to %{{.*}} step 3
// CHECK: affine.for %{{.*}} = 0 to %{{.*}} step 3
// CHECK-NOT: affine.for %{{.*}} = 0 to %[[K]] step 3
// CHECK: affine.for %{{.*}} = 0 to %[[K]] iter_args
// CHECK-NOT: d_tensor.size.mul
// CHECK-NOT: d_affine.for

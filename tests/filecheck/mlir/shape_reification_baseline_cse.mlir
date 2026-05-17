// RUN: mlir-opt %S/../../../experiments/design_benchmarks/shape_reification_benchmark/ordinary_dynamic_shape_same_shape_different_ssa.mlir --canonicalize --cse --symbol-dce | filecheck %s

// CHECK-LABEL: func.func @ordinary_same_shape_different_ssa
// CHECK: tensor.dim %arg0
// CHECK: tensor.dim %arg0
// CHECK: tensor.dim %arg1
// CHECK: tensor.dim %arg1
// CHECK: tensor.dim %arg2
// CHECK: tensor.dim %arg2
// CHECK: tensor.dim %arg3
// CHECK: tensor.dim %arg3
// CHECK: tensor.dim %arg4
// CHECK: tensor.dim %arg4
// CHECK: tensor.dim %arg5
// CHECK: tensor.dim %arg5
// CHECK: arith.muli
// CHECK: arith.muli
// CHECK: arith.muli
// CHECK: arith.muli
// CHECK: arith.muli
// CHECK: arith.muli

// RUN: scair-opt %s -p canonicalize,cse | filecheck %s

builtin.module {
  llvm.func @add_zero(%arg0: i64, %arg1: i64) -> i64 {
    %zero = "llvm.mlir.constant"() <{value = 0 : i64}> : () -> i64
    %lhs = "llvm.add"(%arg0, %zero) : (i64, i64) -> i64
    %rhs = "llvm.add"(%zero, %arg1) : (i64, i64) -> i64
    %sum = "llvm.add"(%lhs, %rhs) : (i64, i64) -> i64
    "llvm.return"(%sum) : (i64) -> ()
  }
}

// CHECK-LABEL: llvm.func @add_zero(
// CHECK-SAME: %[[ARG0:[0-9]+]]: i64
// CHECK-SAME: %[[ARG1:[0-9]+]]: i64
// CHECK-NOT: llvm.mlir.constant
// CHECK-NOT: llvm.add{{.*}}0
// CHECK: %[[SUM:[0-9]+]] = "llvm.add"(%[[ARG0]], %[[ARG1]]) : (i64, i64) -> i64
// CHECK: "llvm.return"(%[[SUM]]) : (i64) -> ()

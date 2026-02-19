// Purpose: Pipeline idempotence on symbolic dim algebra with deep-RAUW into all tensor types.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE1
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce,tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE2

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %o = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat

  %m0 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %m1 = "dtensor.nat.mul"(%m0, %o) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %m2 = "dtensor.nat.add"(%m1, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %k0 = "dtensor.nat.add"(%k, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %k1 = "dtensor.nat.mul"(%k0, %o) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %lhs = "dtensor.empty"() : () -> !dtensor.tensor<[%m2, %k1], f32>
  %rhs = "dtensor.empty"() : () -> !dtensor.tensor<[%k1, %n], f32>
  %mm = "dtensor.matmul"(%lhs, %rhs)
    : (!dtensor.tensor<[%m2, %k1], f32>, !dtensor.tensor<[%k1, %n], f32>) -> !dtensor.tensor<[%m2, %n], f32>
  %bias = "dtensor.empty"() : () -> !dtensor.tensor<[%m2, %n], f32>
  %sum = "dtensor.add"(%mm, %bias)
    : (!dtensor.tensor<[%m2, %n], f32>, !dtensor.tensor<[%m2, %n], f32>) -> !dtensor.tensor<[%m2, %n], f32>
  %prod = "dtensor.mul"(%sum, %sum)
    : (!dtensor.tensor<[%m2, %n], f32>, !dtensor.tensor<[%m2, %n], f32>) -> !dtensor.tensor<[%m2, %n], f32>
  %out = "dtensor.cast"(%prod)
    : (!dtensor.tensor<[%m2, %n], f32>) -> !dtensor.tensor<[%m2, %n], f32>
}

// VERIFY: "dtensor.nat.add"
// VERIFY: "dtensor.nat.mul"
// VERIFY: "dtensor.matmul"
// VERIFY: "dtensor.cast"

// CANON-NOT: "dtensor.nat.add"
// CANON-NOT: "dtensor.nat.mul"

// PIPE1-NOT: "dtensor.nat.add"
// PIPE1-NOT: "dtensor.nat.mul"
// PIPE1-NOT: "dtensor.matmul"
// PIPE1: %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE1: %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE1: %2 = "dtensor.nat.param"() : () -> !dtensor.nat

// PIPE2-NOT: "dtensor.nat.add"
// PIPE2-NOT: "dtensor.nat.mul"
// PIPE2-NOT: "dtensor.matmul"
// PIPE2: %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE2: %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE2: %2 = "dtensor.nat.param"() : () -> !dtensor.nat

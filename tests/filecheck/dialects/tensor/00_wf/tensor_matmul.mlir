// Purpose: Isolated high-level dtensor.matmul verifier coverage, without kernel d_memref IR.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG

// Positive: MxK * KxN produces MxN with SSA-identical inner dims.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %lhs = "test.lhs"() : () -> !dtensor.tensor<[%m, %k], f32>
  %rhs = "test.rhs"() : () -> !dtensor.tensor<[%k, %n], f32>
  %res = "dtensor.matmul"(%lhs, %rhs)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  "test.keep_matmul"(%res) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// VERIFY-LABEL: builtin.module {
// VERIFY: [[M:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY: [[K:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY: [[N:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY: [[LHS:%[0-9]+]] = "test.lhs"() : () -> !dtensor.tensor<{{\[}}[[M]], [[K]]], f32>
// VERIFY: [[RHS:%[0-9]+]] = "test.rhs"() : () -> !dtensor.tensor<{{\[}}[[K]], [[N]]], f32>
// VERIFY: [[RES:%[0-9]+]] = "dtensor.matmul"([[LHS]], [[RHS]]) : (!dtensor.tensor<{{\[}}[[M]], [[K]]], f32>, !dtensor.tensor<{{\[}}[[K]], [[N]]], f32>) -> !dtensor.tensor<{{\[}}[[M]], [[N]]], f32>
// VERIFY: "test.keep_matmul"([[RES]]) : (!dtensor.tensor<{{\[}}[[M]], [[N]]], f32>) -> ()
// VERIFY: }

// -----

// Negative: operands must both be rank-2 tensors.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %lhs = "test.lhs"() : () -> !dtensor.tensor<[%m], f32>
  %rhs = "test.rhs"() : () -> !dtensor.tensor<[%k, %n], f32>
  // expected-error @below {{dtensor.matmul: expected rank-2 operands}}
  %bad = "dtensor.matmul"(%lhs, %rhs)
    : (!dtensor.tensor<[%m], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// DIAG: dtensor.matmul: expected rank-2 operands

// -----

// Negative: lhs inner dim and rhs inner dim must be the same SSA value.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %k1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %lhs = "test.lhs"() : () -> !dtensor.tensor<[%m, %k0], f32>
  %rhs = "test.rhs"() : () -> !dtensor.tensor<[%k1, %n], f32>
  // expected-error @below {{dtensor.matmul: expected SSA-identical inner dims}}
  %bad = "dtensor.matmul"(%lhs, %rhs)
    : (!dtensor.tensor<[%m, %k0], f32>, !dtensor.tensor<[%k1, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// DIAG: dtensor.matmul: expected SSA-identical inner dims (lhs.k === rhs.k)

// -----

// Negative: result dims must be lhs outer and rhs outer dims.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %x = "dtensor.nat.param"() : () -> !dtensor.nat
  %lhs = "test.lhs"() : () -> !dtensor.tensor<[%m, %k], f32>
  %rhs = "test.rhs"() : () -> !dtensor.tensor<[%k, %n], f32>
  // expected-error @below {{dtensor.matmul: expected result dims to be outer dims}}
  %bad = "dtensor.matmul"(%lhs, %rhs)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%x, %n], f32>
}

// DIAG: dtensor.matmul: expected result dims to be outer dims (lhs.m, rhs.n)

// -----

// Negative: lhs, rhs, and result element types must match.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %lhs = "test.lhs"() : () -> !dtensor.tensor<[%m, %k], f32>
  %rhs = "test.rhs"() : () -> !dtensor.tensor<[%k, %n], i32>
  // expected-error @below {{dtensor.matmul: expected equal element types for lhs/rhs/result}}
  %bad = "dtensor.matmul"(%lhs, %rhs)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], i32>) -> !dtensor.tensor<[%m, %n], f32>
}

// DIAG: dtensor.matmul: expected equal element types for lhs/rhs/result

// Purpose: Isolated high-level d_tensor.matmul verifier coverage, without kernel d_memref IR.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG

// Positive: MxK * KxN produces MxN with SSA-identical inner dims.
builtin.module {
  %m = "test.index"() : () -> index
  %k = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %lhs = "test.lhs"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %rhs = "test.rhs"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %res = "d_tensor.matmul"(%lhs, %rhs)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep_matmul"(%res) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// VERIFY-LABEL: builtin.module {
// VERIFY: [[M:%[0-9]+]] = "test.index"() : () -> index
// VERIFY: [[K:%[0-9]+]] = "test.index"() : () -> index
// VERIFY: [[N:%[0-9]+]] = "test.index"() : () -> index
// VERIFY: [[LHS:%[0-9]+]] = "test.lhs"() : () -> !d_tensor.tensor<{{\[}}[[M]], [[K]]], f32>
// VERIFY: [[RHS:%[0-9]+]] = "test.rhs"() : () -> !d_tensor.tensor<{{\[}}[[K]], [[N]]], f32>
// VERIFY: [[RES:%[0-9]+]] = "d_tensor.matmul"([[LHS]], [[RHS]]) : (!d_tensor.tensor<{{\[}}[[M]], [[K]]], f32>, !d_tensor.tensor<{{\[}}[[K]], [[N]]], f32>) -> !d_tensor.tensor<{{\[}}[[M]], [[N]]], f32>
// VERIFY: "test.keep_matmul"([[RES]]) : (!d_tensor.tensor<{{\[}}[[M]], [[N]]], f32>) -> ()
// VERIFY: }

// -----

// Negative: operands must both be rank-2 tensors.
builtin.module {
  %m = "test.index"() : () -> index
  %k = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %lhs = "test.lhs"() : () -> !d_tensor.tensor<[%m], f32>
  %rhs = "test.rhs"() : () -> !d_tensor.tensor<[%k, %n], f32>
  // expected-error @below {{d_tensor.matmul: expected rank-2 operands}}
  %bad = "d_tensor.matmul"(%lhs, %rhs)
    : (!d_tensor.tensor<[%m], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// DIAG: d_tensor.matmul: expected rank-2 operands

// -----

// Negative: lhs inner dim and rhs inner dim must be the same SSA value.
builtin.module {
  %m = "test.index"() : () -> index
  %k0 = "test.index"() : () -> index
  %k1 = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %lhs = "test.lhs"() : () -> !d_tensor.tensor<[%m, %k0], f32>
  %rhs = "test.rhs"() : () -> !d_tensor.tensor<[%k1, %n], f32>
  // expected-error @below {{d_tensor.matmul: expected SSA-identical inner dims}}
  %bad = "d_tensor.matmul"(%lhs, %rhs)
    : (!d_tensor.tensor<[%m, %k0], f32>, !d_tensor.tensor<[%k1, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// DIAG: d_tensor.matmul: expected SSA-identical inner dims (lhs.k === rhs.k)

// -----

// Negative: result dims must be lhs outer and rhs outer dims.
builtin.module {
  %m = "test.index"() : () -> index
  %k = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %x = "test.index"() : () -> index
  %lhs = "test.lhs"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %rhs = "test.rhs"() : () -> !d_tensor.tensor<[%k, %n], f32>
  // expected-error @below {{d_tensor.matmul: expected result dims to be outer dims}}
  %bad = "d_tensor.matmul"(%lhs, %rhs)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%x, %n], f32>
}

// DIAG: d_tensor.matmul: expected result dims to be outer dims (lhs.m, rhs.n)

// -----

// Negative: lhs, rhs, and result element types must match.
builtin.module {
  %m = "test.index"() : () -> index
  %k = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %lhs = "test.lhs"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %rhs = "test.rhs"() : () -> !d_tensor.tensor<[%k, %n], i32>
  // expected-error @below {{d_tensor.matmul: expected equal element types for lhs/rhs/result}}
  %bad = "d_tensor.matmul"(%lhs, %rhs)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], i32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// DIAG: d_tensor.matmul: expected equal element types for lhs/rhs/result

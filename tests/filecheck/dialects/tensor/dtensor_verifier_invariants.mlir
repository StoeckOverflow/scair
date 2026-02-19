// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=PARSE

// Valid anchor for parser/printer sanity.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %d = "dtensor.dim"(%a) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.nat
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "dtensor.nat.param"()
// VERIFY: "dtensor.dim"
// VERIFY: }

// -----

// dtensor.fill operand/elem mismatch.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %v = "test.scalar"() : () -> f32
  // expected-error @below {{dtensor.fill: expected fill value type}}
  %bad = "dtensor.fill"(%v) : (f32) -> !dtensor.tensor<[%p], i32>
}

// -----

// dtensor.dim axis type must be i32.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%p], f32>
  // expected-error @below {{dtensor.dim: expected i32 axis attribute}}
  %bad = "dtensor.dim"(%a) <{axis = 0 : i64}> : (!dtensor.tensor<[%p], f32>) -> !dtensor.nat
}

// -----

// dtensor.dim axis bounds check.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  // expected-error @below {{dtensor.dim: axis 2 out of bounds for rank 2}}
  %bad = "dtensor.dim"(%a) <{axis = 2 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.nat
}

// -----

// dtensor.add rank mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m, %n], f32>
  // expected-error @below {{dtensor.add: expected equal ranks for lhs/rhs}}
  %bad = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m], f32>
}

// -----

// dtensor.add element mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m], i32>
  // expected-error @below {{dtensor.add: expected equal element types for lhs/rhs}}
  %bad = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m], f32>, !dtensor.tensor<[%m], i32>) -> !dtensor.tensor<[%m], f32>
}

// -----

// dtensor.matmul rank check.
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

// -----

// dtensor.matmul element mismatch.
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

// -----

// dtensor.matmul result dims must be outer dims.
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

// -----

// dtensor.matmul inner dim SSA identity.
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

// -----

// dtensor.cast rank mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %src = "test.src"() : () -> !dtensor.tensor<[%m], f32>
  // expected-error @below {{dtensor.cast: expected equal ranks}}
  %bad = "dtensor.cast"(%src) : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// -----

// dtensor.cast element mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %src = "test.src"() : () -> !dtensor.tensor<[%m], f32>
  // expected-error @below {{dtensor.cast: expected equal element types}}
  %bad = "dtensor.cast"(%src) : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%m], i32>
}

// -----

// nat.add operand sort check.
builtin.module {
  %x = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  // expected-error @below {{dtensor.nat.add}}
  // expected-error @below {{!dtensor.nat}}
  %bad = "dtensor.nat.add"(%x, %m) : (i32, !dtensor.nat) -> !dtensor.nat
}

// -----

// nat.mul operand sort check.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %x = "arith.constant"() <{value = 8 : i32}> : () -> i32
  // expected-error @below {{dtensor.nat.mul}}
  // expected-error @below {{!dtensor.nat}}
  %bad = "dtensor.nat.mul"(%m, %x) : (!dtensor.nat, i32) -> !dtensor.nat
}

// -----

// Parser negatives: matrix arity mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %x = "test.bad"() : () -> !dtensor.matrix<%m, f32>
}

// expected-error @above {{Parse error}}
// PARSE: Parse error at [[FILE]]

// -----

// Parser negatives: malformed tensor punctuation.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %x = "test.bad"() : () -> !dtensor.tensor<[%m %n], f32>
}

// expected-error @above {{Parse error}}
// PARSE: Parse error at [[FILE]]

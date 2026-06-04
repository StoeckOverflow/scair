// Purpose: Canonical negative verifier coverage + pass safety (transform and must-not-transform) in one place.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefixes=CANON,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p canonicalize | filecheck %s -DFILE=%s --check-prefixes=CN,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefixes=CSE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefixes=DCE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefixes=PIPE,DIAG

// Smoke: pass pipeline keeps a simple valid tensor program intact.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  "test.keep_smoke"(%a) : (!dtensor.tensor<[%m], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY:   %1 = "test.a"() : () -> !dtensor.tensor<[%0], f32>
// VERIFY:   "test.keep_smoke"(%1) : (!dtensor.tensor<[%0], f32>) -> ()
// VERIFY: }
// CANON: builtin.module {
// CANON:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON:   %1 = "test.a"() : () -> !dtensor.tensor<[%0], f32>
// CANON:   "test.keep_smoke"(%1) : (!dtensor.tensor<[%0], f32>) -> ()
// CANON: }
// CN: builtin.module {
// CN:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CN:   %1 = "test.a"() : () -> !dtensor.tensor<[%0], f32>
// CN:   "test.keep_smoke"(%1) : (!dtensor.tensor<[%0], f32>) -> ()
// CN: }
// CSE: builtin.module {
// CSE:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CSE:   %1 = "test.a"() : () -> !dtensor.tensor<[%0], f32>
// CSE:   "test.keep_smoke"(%1) : (!dtensor.tensor<[%0], f32>) -> ()
// CSE: }
// DCE: builtin.module {
// DCE:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// DCE:   %1 = "test.a"() : () -> !dtensor.tensor<[%0], f32>
// DCE:   "test.keep_smoke"(%1) : (!dtensor.tensor<[%0], f32>) -> ()
// DCE: }
// PIPE: builtin.module {
// PIPE:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE:   %1 = "test.a"() : () -> !dtensor.tensor<[%0], f32>
// PIPE:   "test.keep_smoke"(%1) : (!dtensor.tensor<[%0], f32>) -> ()
// PIPE: }

// -----

// Invalid: matmul rank mismatch.
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

// Invalid: matmul inner dims not SSA-identical.
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

// DIAG: dtensor.matmul: expected SSA-identical inner dims

// -----

// Invalid: matmul result dims not outer dims.
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

// DIAG: dtensor.matmul: expected result dims to be outer dims

// -----

// Invalid: add element mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m], i32>
  // expected-error @below {{dtensor.add: expected equal element types for lhs/rhs}}
  %bad = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m], f32>, !dtensor.tensor<[%m], i32>) -> !dtensor.tensor<[%m], f32>
}

// DIAG: dtensor.add: expected equal element types for lhs/rhs

// -----

// Invalid: add rank mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m, %n], f32>
  // expected-error @below {{dtensor.add: expected equal ranks for lhs/rhs}}
  %bad = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m], f32>
}

// DIAG: dtensor.add: expected equal ranks for lhs/rhs

// -----

// Invalid: add dims not SSA-identical.
builtin.module {
  %m0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %m1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m0], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m1], f32>
  // expected-error @below {{dtensor.add: expected pairwise SSA-identical dims for lhs/rhs}}
  %bad = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m0], f32>, !dtensor.tensor<[%m1], f32>) -> !dtensor.tensor<[%m0], f32>
}

// DIAG: dtensor.add: expected pairwise SSA-identical dims for lhs/rhs

// -----

// Invalid: mul element mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m], i32>
  // expected-error @below {{dtensor.mul: expected equal element types for lhs/rhs}}
  %bad = "dtensor.mul"(%a, %b)
    : (!dtensor.tensor<[%m], f32>, !dtensor.tensor<[%m], i32>) -> !dtensor.tensor<[%m], f32>
}

// DIAG: dtensor.mul: expected equal element types for lhs/rhs

// -----

// Invalid: mul rank mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m, %n], f32>
  // expected-error @below {{dtensor.mul: expected equal ranks for lhs/rhs}}
  %bad = "dtensor.mul"(%a, %b)
    : (!dtensor.tensor<[%m], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m], f32>
}

// DIAG: dtensor.mul: expected equal ranks for lhs/rhs

// -----

// Invalid: mul dims not SSA-identical.
builtin.module {
  %m0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %m1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m0], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m1], f32>
  // expected-error @below {{dtensor.mul: expected pairwise SSA-identical dims for lhs/rhs}}
  %bad = "dtensor.mul"(%a, %b)
    : (!dtensor.tensor<[%m0], f32>, !dtensor.tensor<[%m1], f32>) -> !dtensor.tensor<[%m0], f32>
}

// DIAG: dtensor.mul: expected pairwise SSA-identical dims for lhs/rhs

// -----

// Invalid: cast rank mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %src = "test.src"() : () -> !dtensor.tensor<[%m], f32>
  // expected-error @below {{dtensor.cast: expected equal ranks}}
  %bad = "dtensor.cast"(%src) : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// DIAG: dtensor.cast: expected equal ranks

// -----

// Invalid: cast element mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %src = "test.src"() : () -> !dtensor.tensor<[%m], f32>
  // expected-error @below {{dtensor.cast: expected equal element types}}
  %bad = "dtensor.cast"(%src) : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%m], i32>
}

// DIAG: dtensor.cast: expected equal element types

// -----

// Invalid: cast dims mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %d0 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %d1 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %src = "test.src"() : () -> !dtensor.tensor<[%d0], f32>
  // expected-error @below {{dtensor.cast: expected pairwise SSA-identical dims}}
  %bad = "dtensor.cast"(%src) : (!dtensor.tensor<[%d0], f32>) -> !dtensor.tensor<[%d1], f32>
}

// DIAG: dtensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid: dim axis = -1.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  // expected-error @below {{dtensor.dim: axis -1 out of bounds for rank 2}}
  %bad = "dtensor.dim"(%a) <{axis = -1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// DIAG: dtensor.dim: axis -1 out of bounds for rank 2

// -----

// Invalid: dim axis == rank.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  // expected-error @below {{dtensor.dim: axis 2 out of bounds for rank 2}}
  %bad = "dtensor.dim"(%a) <{axis = 2 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// DIAG: dtensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid: dim axis attribute type.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  // expected-error @below {{dtensor.dim: expected i32 axis attribute}}
  %bad = "dtensor.dim"(%a) <{axis = 0 : i64}> : (!dtensor.tensor<[%m], f32>) -> !value<%m>
}

// DIAG: dtensor.dim: expected i32 axis attribute

// -----

// Invalid: nat.const negative literal.
builtin.module {
  // expected-error @below {{dtensor.nat.const: expected non-negative literal}}
  %n = "dtensor.nat.const"() <{value = -1 : i32}> : () -> !dtensor.nat
}

// DIAG: dtensor.nat.const: expected non-negative literal

// -----

// Invalid: posnat const must be strictly positive.
builtin.module {
  // expected-error @below {{dtensor.nat.const: expected positive literal for !dtensor.posnat}}
  %n = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.posnat
}

// DIAG: dtensor.nat.const: expected positive literal for !dtensor.posnat

// -----

// Invalid: posnat sum requires at least one positive operand.
builtin.module {
  %lhs = "dtensor.nat.param"() : () -> !dtensor.nat
  %rhs = "dtensor.nat.param"() : () -> !dtensor.nat
  // expected-error @below {{dtensor.nat.add: !dtensor.posnat result requires at least one !dtensor.posnat operand}}
  %bad = "dtensor.nat.add"(%lhs, %rhs) : (!dtensor.nat, !dtensor.nat) -> !dtensor.posnat
}

// DIAG: dtensor.nat.add: !dtensor.posnat result requires at least one !dtensor.posnat operand

// -----

// Invalid: posnat product requires positive operands.
builtin.module {
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %p = "dtensor.nat.param"() : () -> !dtensor.posnat
  // expected-error @below {{dtensor.nat.mul: !dtensor.posnat result requires two !dtensor.posnat operands}}
  %bad = "dtensor.nat.mul"(%n, %p) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.posnat
}

// DIAG: dtensor.nat.mul: !dtensor.posnat result requires two !dtensor.posnat operands

// -----

// Invalid: refine_positive proof must be i1.
builtin.module {
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %proof = "arith.constant"() <{value = 1 : i32}> : () -> i32
  // expected-error @below {{dtensor.nat.refine_positive: expected i1 proof}}
  %bad = "dtensor.nat.refine_positive"(%n, %proof) : (!dtensor.nat, i32) -> !dtensor.posnat
}

// DIAG: dtensor.nat.refine_positive: expected i1 proof

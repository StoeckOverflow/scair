// Purpose: Canonical negative verifier coverage + pass safety (transform and must-not-transform) in one place.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p canonicalize | filecheck %s -DFILE=%s --check-prefix=CN
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

// Pass-safety: should transform canonical nat identities and keep IR valid.
builtin.module {
  %x = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %o = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  %sx = "dtensor.nat.add"(%x, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %d = "dtensor.nat.mul"(%sx, %o) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %a = "test.a"() : () -> !dtensor.tensor<[%d], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%d], f32>
  %s = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%d], f32>, !dtensor.tensor<[%d], f32>) -> !dtensor.tensor<[%d], f32>
  "test.keep_pass_ok"(%s) : (!dtensor.tensor<[%d], f32>) -> ()
}

// VERIFY: "test.keep_pass_ok"
// CANON-NOT: "dtensor.nat.add"
// CANON-NOT: "dtensor.nat.mul"
// CANON: "test.keep_pass_ok"
// CN: "test.keep_pass_ok"
// CSE: "test.keep_pass_ok"
// DCE: "test.keep_pass_ok"
// PIPE-NOT: "dtensor.nat.add"
// PIPE-NOT: "dtensor.nat.mul"
// PIPE: "test.keep_pass_ok"

// -----

// Pass-safety: must-not-transform CSE case (distinct dim identities).
builtin.module {
  %p0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %p1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %e0 = "dtensor.empty"() : () -> !dtensor.tensor<[%p0], f32>
  %e1 = "dtensor.empty"() : () -> !dtensor.tensor<[%p1], f32>
  "test.keep_distinct0"(%e0) : (!dtensor.tensor<[%p0], f32>) -> ()
  "test.keep_distinct1"(%e1) : (!dtensor.tensor<[%p1], f32>) -> ()
}

// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[%1], f32>
// PIPE: "test.keep_distinct0"
// PIPE: "test.keep_distinct1"

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

// -----

// Invalid: cast rank mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %src = "test.src"() : () -> !dtensor.tensor<[%m], f32>
  // expected-error @below {{dtensor.cast: expected equal ranks}}
  %bad = "dtensor.cast"(%src) : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// -----

// Invalid: cast element mismatch.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %src = "test.src"() : () -> !dtensor.tensor<[%m], f32>
  // expected-error @below {{dtensor.cast: expected equal element types}}
  %bad = "dtensor.cast"(%src) : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%m], i32>
}

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

// -----

// Invalid: dim axis = -1.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  // expected-error @below {{dtensor.dim: axis -1 out of bounds for rank 2}}
  %bad = "dtensor.dim"(%a) <{axis = -1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.nat
}

// -----

// Invalid: dim axis == rank.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  // expected-error @below {{dtensor.dim: axis 2 out of bounds for rank 2}}
  %bad = "dtensor.dim"(%a) <{axis = 2 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.nat
}

// -----

// Invalid: dim axis attribute type.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  // expected-error @below {{dtensor.dim: expected i32 axis attribute}}
  %bad = "dtensor.dim"(%a) <{axis = 0 : i64}> : (!dtensor.tensor<[%m], f32>) -> !dtensor.nat
}

// -----

// Invalid: nat.const negative literal.
builtin.module {
  // expected-error @below {{dtensor.nat.const: expected non-negative literal}}
  %n = "dtensor.nat.const"() <{value = -1 : i32}> : () -> !dtensor.nat
}

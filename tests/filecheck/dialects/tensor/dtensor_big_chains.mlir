// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,cse | filecheck %s -DFILE=%s --check-prefix=CSECAN
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

// Residual-ish elementwise chain with symbolic dims.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %x = "test.x"() : () -> !dtensor.tensor<[%m, %n], f32>

  %v0 = "dtensor.mul"(%x, %x)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %v1 = "dtensor.add"(%v0, %x)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %v2 = "dtensor.mul"(%v1, %v1)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %v3 = "dtensor.add"(%v2, %x)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %out = "dtensor.cast"(%v3)
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>

  "test.keep_residual"(%out) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// VERIFY: "test.keep_residual"
// PIPE: "test.keep_residual"

// -----

// CSE should merge identical tensor subgraphs (same dim SSA identity).
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %x = "test.x"() : () -> !dtensor.tensor<[%m, %n], f32>

  %a0 = "dtensor.mul"(%x, %x)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %a1 = "dtensor.mul"(%x, %x)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %y0 = "dtensor.add"(%a0, %x)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %y1 = "dtensor.add"(%a1, %x)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>

  "test.keep_cse_left"(%y0) : (!dtensor.tensor<[%m, %n], f32>) -> ()
  "test.keep_cse_right"(%y1) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// CSE: "test.keep_cse_left"(%4)
// CSE: "test.keep_cse_right"(%4)

// -----

// Canonicalize-before-CSE matters for type-embedded dims.
builtin.module {
  %x = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %s0 = "dtensor.nat.add"(%x, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %e0 = "dtensor.empty"() : () -> !dtensor.tensor<[%s0], f32>
  %e1 = "dtensor.empty"() : () -> !dtensor.tensor<[%x], f32>

  "test.keep_canon_cse0"(%e0) : (!dtensor.tensor<[%s0], f32>) -> ()
  "test.keep_canon_cse1"(%e1) : (!dtensor.tensor<[%x], f32>) -> ()
}

// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[%2], f32>
// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>

// CSECAN-LABEL: builtin.module {
// CSECAN: [[E:%[0-9]+]] = "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// CSECAN-NOT: "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// CSECAN: "test.keep_canon_cse0"([[E]])
// CSECAN: "test.keep_canon_cse1"([[E]])

// -----

// Linear layer core with dim extraction chain.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat

  %X = "test.X"() : () -> !dtensor.tensor<[%m, %k], f32>
  %W = "test.W"() : () -> !dtensor.tensor<[%k, %n], f32>
  %Y = "dtensor.matmul"(%X, %W)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %Z = "dtensor.mul"(%Y, %Y)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %Out = "dtensor.add"(%Z, %Y)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>

  %d0 = "dtensor.dim"(%Out) <{axis = 0 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.nat
  %d1 = "dtensor.dim"(%Out) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.nat
  %E = "dtensor.empty"() : () -> !dtensor.tensor<[%d0, %d1], f32>
  "test.keep_linear"(%E) : (!dtensor.tensor<[%d0, %d1], f32>) -> ()
}

// VERIFY: "dtensor.dim"
// CANON: "test.keep_linear"(%[[E:[0-9]+]]) : (!dtensor.tensor<[%0, %2], f32>) -> ()

// -----

// Negative: broadcast-like add must fail.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%n], f32>
  // expected-error @below {{dtensor.add: expected equal ranks for lhs/rhs}}
  %bad = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// -----

// Negative: long-chain inner shape mismatch (k0 vs k1, semantically similar but non-identical).
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %k0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %k1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat

  %s0 = "dtensor.nat.add"(%k0, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s1 = "dtensor.nat.add"(%k1, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %A = "test.A"() : () -> !dtensor.tensor<[%m, %s0], f32>
  %B = "test.B"() : () -> !dtensor.tensor<[%s1, %n], f32>
  // expected-error @below {{dtensor.matmul: expected SSA-identical inner dims}}
  %bad = "dtensor.matmul"(%A, %B)
    : (!dtensor.tensor<[%m, %s0], f32>, !dtensor.tensor<[%s1, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// -----

// DCE behavior: keep type-only dim uses through a live tensor user, remove dead nat algebra.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %t = "dtensor.empty"() : () -> !dtensor.tensor<[%p], f32>
  %u = "test.id"(%t) : (!dtensor.tensor<[%p], f32>) -> !dtensor.tensor<[%p], f32>
  "test.keep_dce_big"(%u) : (!dtensor.tensor<[%p], f32>) -> ()

  %c4 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %c0 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %dead0 = "dtensor.nat.add"(%c4, %c0) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %dead1 = "dtensor.nat.mul"(%dead0, %c4) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
}

// DCE-LABEL: builtin.module {
// DCE: "test.keep_dce_big"
// DCE-NOT: "dtensor.nat.add"(%3, %4)
// DCE-NOT: "dtensor.nat.mul"(%5, %3)

// Purpose: Canonical negative verifier coverage + pass safety (transform and must-not-transform) in one place.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefixes=CANON,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p canonicalize | filecheck %s -DFILE=%s --check-prefixes=CN,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefixes=CSE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefixes=DCE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefixes=PIPE,DIAG

// Smoke: pass pipeline keeps a simple valid tensor program intact.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  "test.keep_smoke"(%a) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// VERIFY:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// VERIFY:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// VERIFY: }
// CANON: builtin.module {
// CANON:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CANON:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// CANON:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// CANON: }
// CN: builtin.module {
// CN:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CN:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// CN:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// CN: }
// CSE: builtin.module {
// CSE:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CSE:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// CSE:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// CSE: }
// DCE: builtin.module {
// DCE:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// DCE:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// DCE:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// DCE: }
// PIPE: builtin.module {
// PIPE:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// PIPE:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// PIPE: }

// -----

// Invalid: matmul rank mismatch.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %k = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %lhs = "test.lhs"() : () -> !d_tensor.tensor<[%m], f32>
  %rhs = "test.rhs"() : () -> !d_tensor.tensor<[%k, %n], f32>
  // expected-error @below {{d_tensor.matmul: expected rank-2 operands}}
  %bad = "d_tensor.matmul"(%lhs, %rhs)
    : (!d_tensor.tensor<[%m], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// DIAG: d_tensor.matmul: expected rank-2 operands

// -----

// Invalid: matmul inner dims not SSA-identical.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %k1 = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %lhs = "test.lhs"() : () -> !d_tensor.tensor<[%m, %k0], f32>
  %rhs = "test.rhs"() : () -> !d_tensor.tensor<[%k1, %n], f32>
  // expected-error @below {{d_tensor.matmul: expected SSA-identical inner dims}}
  %bad = "d_tensor.matmul"(%lhs, %rhs)
    : (!d_tensor.tensor<[%m, %k0], f32>, !d_tensor.tensor<[%k1, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// DIAG: d_tensor.matmul: expected SSA-identical inner dims

// -----

// Invalid: matmul result dims not outer dims.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %k = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %x = "d_tensor.size.param"() : () -> !d_tensor.size
  %lhs = "test.lhs"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %rhs = "test.rhs"() : () -> !d_tensor.tensor<[%k, %n], f32>
  // expected-error @below {{d_tensor.matmul: expected result dims to be outer dims}}
  %bad = "d_tensor.matmul"(%lhs, %rhs)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%x, %n], f32>
}

// DIAG: d_tensor.matmul: expected result dims to be outer dims

// -----

// Invalid: add element mismatch.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m], i32>
  // expected-error @below {{d_tensor.add: expected equal element types for lhs/rhs}}
  %bad = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%m], f32>, !d_tensor.tensor<[%m], i32>) -> !d_tensor.tensor<[%m], f32>
}

// DIAG: d_tensor.add: expected equal element types for lhs/rhs

// -----

// Invalid: add rank mismatch.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m, %n], f32>
  // expected-error @below {{d_tensor.add: expected equal ranks for lhs/rhs}}
  %bad = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%m], f32>, !d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m], f32>
}

// DIAG: d_tensor.add: expected equal ranks for lhs/rhs

// -----

// Invalid: add dims not SSA-identical.
builtin.module {
  %m0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %m1 = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m0], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m1], f32>
  // expected-error @below {{d_tensor.add: expected pairwise SSA-identical dims for lhs/rhs}}
  %bad = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%m0], f32>, !d_tensor.tensor<[%m1], f32>) -> !d_tensor.tensor<[%m0], f32>
}

// DIAG: d_tensor.add: expected pairwise SSA-identical dims for lhs/rhs

// -----

// Invalid: mul element mismatch.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m], i32>
  // expected-error @below {{d_tensor.mul: expected equal element types for lhs/rhs}}
  %bad = "d_tensor.mul"(%a, %b)
    : (!d_tensor.tensor<[%m], f32>, !d_tensor.tensor<[%m], i32>) -> !d_tensor.tensor<[%m], f32>
}

// DIAG: d_tensor.mul: expected equal element types for lhs/rhs

// -----

// Invalid: mul rank mismatch.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m, %n], f32>
  // expected-error @below {{d_tensor.mul: expected equal ranks for lhs/rhs}}
  %bad = "d_tensor.mul"(%a, %b)
    : (!d_tensor.tensor<[%m], f32>, !d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m], f32>
}

// DIAG: d_tensor.mul: expected equal ranks for lhs/rhs

// -----

// Invalid: mul dims not SSA-identical.
builtin.module {
  %m0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %m1 = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m0], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m1], f32>
  // expected-error @below {{d_tensor.mul: expected pairwise SSA-identical dims for lhs/rhs}}
  %bad = "d_tensor.mul"(%a, %b)
    : (!d_tensor.tensor<[%m0], f32>, !d_tensor.tensor<[%m1], f32>) -> !d_tensor.tensor<[%m0], f32>
}

// DIAG: d_tensor.mul: expected pairwise SSA-identical dims for lhs/rhs

// -----

// Invalid: cast rank mismatch.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  // expected-error @below {{d_tensor.cast: expected equal ranks}}
  %bad = "d_tensor.cast"(%src) : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// DIAG: d_tensor.cast: expected equal ranks

// -----

// Invalid: cast element mismatch.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  // expected-error @below {{d_tensor.cast: expected equal element types}}
  %bad = "d_tensor.cast"(%src) : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], i32>
}

// DIAG: d_tensor.cast: expected equal element types

// -----

// Invalid: cast dims mismatch.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %z = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %d0 = "d_tensor.size.add"(%m, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %d1 = "d_tensor.size.add"(%m, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%d0], f32>
  // expected-error @below {{d_tensor.cast: expected pairwise SSA-identical dims}}
  %bad = "d_tensor.cast"(%src) : (!d_tensor.tensor<[%d0], f32>) -> !d_tensor.tensor<[%d1], f32>
}

// DIAG: d_tensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid: dim axis = -1.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  // expected-error @below {{d_tensor.dim: axis -1 out of bounds for rank 2}}
  %bad = "d_tensor.dim"(%a) <{axis = -1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// DIAG: d_tensor.dim: axis -1 out of bounds for rank 2

// -----

// Invalid: dim axis == rank.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  // expected-error @below {{d_tensor.dim: axis 2 out of bounds for rank 2}}
  %bad = "d_tensor.dim"(%a) <{axis = 2 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// DIAG: d_tensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid: dim axis attribute type.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  // expected-error @below {{d_tensor.dim: expected i32 axis attribute}}
  %bad = "d_tensor.dim"(%a) <{axis = 0 : i64}> : (!d_tensor.tensor<[%m], f32>) -> !value<%m>
}

// DIAG: d_tensor.dim: expected i32 axis attribute

// -----

// Invalid: nat.const negative literal.
builtin.module {
  // expected-error @below {{d_tensor.size.constant: expected non-negative literal}}
  %n = "d_tensor.size.constant"() <{value = -1 : i32}> : () -> !d_tensor.size
}

// DIAG: d_tensor.size.constant: expected non-negative literal

// -----

// Invalid: pos_size const must be strictly positive.
builtin.module {
  // expected-error @below {{d_tensor.size.constant: expected positive literal for !d_tensor.pos_size}}
  %n = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.pos_size
}

// DIAG: d_tensor.size.constant: expected positive literal for !d_tensor.pos_size

// -----

// Invalid: pos_size sum requires at least one positive operand.
builtin.module {
  %lhs = "d_tensor.size.param"() : () -> !d_tensor.size
  %rhs = "d_tensor.size.param"() : () -> !d_tensor.size
  // expected-error @below {{d_tensor.size.add: !d_tensor.pos_size result requires at least one !d_tensor.pos_size operand}}
  %bad = "d_tensor.size.add"(%lhs, %rhs) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.pos_size
}

// DIAG: d_tensor.size.add: !d_tensor.pos_size result requires at least one !d_tensor.pos_size operand

// -----

// Invalid: pos_size product requires positive operands.
builtin.module {
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %p = "d_tensor.size.param"() : () -> !d_tensor.pos_size
  // expected-error @below {{d_tensor.size.mul: !d_tensor.pos_size result requires two !d_tensor.pos_size operands}}
  %bad = "d_tensor.size.mul"(%n, %p) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.pos_size
}

// DIAG: d_tensor.size.mul: !d_tensor.pos_size result requires two !d_tensor.pos_size operands

// -----

// Invalid: refine_positive proof must be a dedicated proof token.
builtin.module {
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %proof = "arith.constant"() <{value = 1 : i32}> : () -> i32
  // expected-error @below {{d_tensor.size.refine_positive: expected proof from d_tensor.size.positive_proof}}
  %bad = "d_tensor.size.refine_positive"(%n, %proof) : (!d_tensor.size, i32) -> !d_tensor.pos_size
}

// DIAG: d_tensor.size.refine_positive: expected proof from d_tensor.size.positive_proof

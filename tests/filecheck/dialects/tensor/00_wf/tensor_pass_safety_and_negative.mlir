// Purpose: Canonical negative verifier coverage + pass safety (transform and must-not-transform) in one place.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefixes=CANON,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p canonicalize | filecheck %s -DFILE=%s --check-prefixes=CN,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefixes=CSE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefixes=DCE,DIAG
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefixes=PIPE,DIAG

// Smoke: pass pipeline keeps a simple valid tensor program intact.
builtin.module {
  %m = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  "test.keep_smoke"(%a) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.index"() : () -> index
// VERIFY:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// VERIFY:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// VERIFY: }
// CANON: builtin.module {
// CANON:   %0 = "test.index"() : () -> index
// CANON:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// CANON:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// CANON: }
// CN: builtin.module {
// CN:   %0 = "test.index"() : () -> index
// CN:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// CN:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// CN: }
// CSE: builtin.module {
// CSE:   %0 = "test.index"() : () -> index
// CSE:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// CSE:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// CSE: }
// DCE: builtin.module {
// DCE:   %0 = "test.index"() : () -> index
// DCE:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// DCE:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// DCE: }
// PIPE: builtin.module {
// PIPE:   %0 = "test.index"() : () -> index
// PIPE:   %1 = "test.a"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE:   "test.keep_smoke"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// PIPE: }

// -----

// Invalid: matmul rank mismatch.
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

// Invalid: matmul inner dims not SSA-identical.
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

// DIAG: d_tensor.matmul: expected SSA-identical inner dims

// -----

// Invalid: matmul result dims not outer dims.
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

// DIAG: d_tensor.matmul: expected result dims to be outer dims

// -----

// Invalid: add element mismatch.
builtin.module {
  %m = "test.index"() : () -> index
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
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
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
  %m0 = "test.index"() : () -> index
  %m1 = "test.index"() : () -> index
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
  %m = "test.index"() : () -> index
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
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
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
  %m0 = "test.index"() : () -> index
  %m1 = "test.index"() : () -> index
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
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  // expected-error @below {{d_tensor.cast: expected equal ranks}}
  %bad = "d_tensor.cast"(%src) : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// DIAG: d_tensor.cast: expected equal ranks

// -----

// Invalid: cast element mismatch.
builtin.module {
  %m = "test.index"() : () -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  // expected-error @below {{d_tensor.cast: expected equal element types}}
  %bad = "d_tensor.cast"(%src) : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], i32>
}

// DIAG: d_tensor.cast: expected equal element types

// -----

// Invalid: cast dims mismatch.
builtin.module {
  %m = "test.index"() : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %d0 = "arith.addi"(%m, %z) : (index, index) -> index
  %d1 = "arith.addi"(%m, %z) : (index, index) -> index
  %src = "test.src"() : () -> !d_tensor.tensor<[%d0], f32>
  // expected-error @below {{d_tensor.cast: expected pairwise SSA-identical dims}}
  %bad = "d_tensor.cast"(%src) : (!d_tensor.tensor<[%d0], f32>) -> !d_tensor.tensor<[%d1], f32>
}

// DIAG: d_tensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid: dim axis = -1.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  // expected-error @below {{d_tensor.dim: axis -1 out of bounds for rank 2}}
  %bad = "d_tensor.dim"(%a) <{axis = -1 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// DIAG: d_tensor.dim: axis -1 out of bounds for rank 2

// -----

// Invalid: dim axis == rank.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  // expected-error @below {{d_tensor.dim: axis 2 out of bounds for rank 2}}
  %bad = "d_tensor.dim"(%a) <{axis = 2 : i32}> : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// DIAG: d_tensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid: dim axis attribute type.
builtin.module {
  %m = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m], f32>
  // expected-error @below {{d_tensor.dim: expected i32 axis attribute}}
  %bad = "d_tensor.dim"(%a) <{axis = 0 : i64}> : (!d_tensor.tensor<[%m], f32>) -> !value<%m>
}

// DIAG: d_tensor.dim: expected i32 axis attribute

// -----

// Valid: dimension validation accepts index-typed SSA values, not numeric proofs.
builtin.module {
  %n = "arith.constant"() <{value = -1 : index}> : () -> index
  %t = "test.t"() : () -> !d_tensor.tensor<[%n], f32>
}

// -----

// Valid: zero index values are accepted as SSA dimensions.
builtin.module {
  %n = "arith.constant"() <{value = 0 : index}> : () -> index
  %t = "test.t"() : () -> !d_tensor.tensor<[%n], f32>
}

// -----

// Valid: arith.addi index result may be used as a dimension.
builtin.module {
  %lhs = "test.index"() : () -> index
  %rhs = "test.index"() : () -> index
  %sum = "arith.addi"(%lhs, %rhs) : (index, index) -> index
  %t = "test.t"() : () -> !d_tensor.tensor<[%sum], f32>
}

// -----

// Valid: arith.muli index result may be used as a dimension.
builtin.module {
  %n = "test.index"() : () -> index
  %p = "test.index"() : () -> index
  %prod = "arith.muli"(%n, %p) : (index, index) -> index
  %t = "test.t"() : () -> !d_tensor.tensor<[%prod], f32>
}

// -----

// Invalid: non-index SSA values cannot be used as tensor dimensions.
builtin.module {
  %bad_dim = "arith.constant"() <{value = 1 : i32}> : () -> i32
  // expected-error @below {{shape SSA parameter must have type index, got i32}}
  %bad = "test.bad"() : () -> !d_tensor.tensor<[%bad_dim], f32>
}

// DIAG: shape SSA parameter must have type index, got i32

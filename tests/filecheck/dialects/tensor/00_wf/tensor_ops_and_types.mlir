// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file --parsing-diagnostics | filecheck %s -DFILE=%s --check-prefix=PARSE

// Valid: nat ops + constructors + elementwise + dim + cast.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
  %k = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %mn = "d_tensor.nat.mul"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %s = "d_tensor.nat.add"(%m, %k) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat

  %zero = "test.zero"() : () -> f32
  %e = "d_tensor.empty"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %f = "d_tensor.fill"(%zero) : (f32) -> !d_tensor.tensor<[%m, %n], f32>
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %sum = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%m, %n], f32>, !d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  %prod = "d_tensor.mul"(%a, %b)
    : (!d_tensor.tensor<[%m, %n], f32>, !d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  %d1 = "d_tensor.dim"(%a) <{axis = 1 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%n>
  %c = "d_tensor.cast"(%a)
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// VERIFY:   %1 = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
// VERIFY:   %2 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
// VERIFY:   %3 = "d_tensor.nat.mul"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:   %4 = "d_tensor.nat.add"(%0, %2) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// VERIFY:   %5 = "test.zero"() : () -> f32
// VERIFY:   %6 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %7 = "d_tensor.fill"(%5) : (f32) -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %8 = "test.a"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %9 = "test.b"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %10 = "d_tensor.add"(%8, %9) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %11 = "d_tensor.mul"(%8, %9) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %12 = "d_tensor.dim"(%8) <{axis = 1 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%1>
// VERIFY:   %13 = "d_tensor.cast"(%8) : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY: }

// -----

// Valid: nat.add may refine to !d_tensor.posnat if either operand is positive.
builtin.module {
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %p = "d_tensor.nat.param"() : () -> !d_tensor.posnat
  %sum0 = "d_tensor.nat.add"(%n, %p) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.posnat
  %sum1 = "d_tensor.nat.add"(%p, %n) : (!d_tensor.posnat, !d_tensor.nat) -> !d_tensor.posnat
  %a = "test.a"() : () -> !d_tensor.tensor<[%sum0], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%sum1], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
// VERIFY:   %2 = "d_tensor.nat.add"(%0, %1) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.posnat
// VERIFY:   %3 = "d_tensor.nat.add"(%1, %0) : (!d_tensor.posnat, !d_tensor.nat) -> !d_tensor.posnat
// VERIFY:   %4 = "test.a"() : () -> !d_tensor.tensor<[%2], f32>
// VERIFY:   %5 = "test.b"() : () -> !d_tensor.tensor<[%3], f32>
// VERIFY: }

// -----

// Valid: refine_positive bridges a nat plus i1 proof into !d_tensor.posnat.
builtin.module {
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %proof = "arith.constant"() <{value = true}> : () -> i1
  %p = "d_tensor.nat.refine_positive"(%n, %proof) : (!d_tensor.nat, i1) -> !d_tensor.posnat
  %idx = "d_tensor.shape.to_index"(%p) : (!d_tensor.posnat) -> index
  %t = "test.t"() : () -> !d_tensor.tensor<[%p], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %1 = "arith.constant"() <{value = true}> : () -> i1
// VERIFY:   %2 = "d_tensor.nat.refine_positive"(%0, %1) : (!d_tensor.nat, i1) -> !d_tensor.posnat
// VERIFY:   %3 = "d_tensor.shape.to_index"(%2) : (!d_tensor.posnat) -> index
// VERIFY:   %4 = "test.t"() : () -> !d_tensor.tensor<[%2], f32>
// VERIFY: }

// -----

// Invalid: nat.const literal must be >= 0.
builtin.module {
  %n = "d_tensor.nat.const"() <{value = -1 : i32}> : () -> !d_tensor.nat
}

// VERIFY: d_tensor.nat.const: expected non-negative literal

// -----

// Invalid: nat.const missing required `value` attribute.
builtin.module {
  %n = "d_tensor.nat.const"() : () -> !d_tensor.nat
}

// VERIFY: Missing required property "value"

// -----

// Invalid: nat.add wrong operand type.
builtin.module {
  %x = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %m = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %r = "d_tensor.nat.add"(%x, %m) : (i32, !d_tensor.nat) -> !d_tensor.nat
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// VERIFY:   %1 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
// VERIFY:   %2 = "d_tensor.nat.add"(%0, %1) : (i32, !d_tensor.nat) -> !d_tensor.nat
// VERIFY: }

// -----

// Invalid: nat.mul wrong operand arity.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %r = "d_tensor.nat.mul"(%m) : (!d_tensor.nat) -> !d_tensor.nat
}

// VERIFY: Expected 2 operands, got 1.

// -----

// Valid: index_to_nat is an explicit bridge from runtime index to shape-domain nat.
builtin.module {
  %idx = "arith.constant"() <{value = 12 : index}> : () -> index
  %nat = "d_tensor.index_to_nat"(%idx) : (index) -> !d_tensor.nat
  %back = "d_tensor.shape.to_index"(%nat) : (!d_tensor.nat) -> index
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 12 : index}> : () -> index
// VERIFY:   %1 = "d_tensor.index_to_nat"(%0) : (index) -> !d_tensor.nat
// VERIFY:   %2 = "d_tensor.shape.to_index"(%1) : (!d_tensor.nat) -> index
// VERIFY: }

// -----

// Invalid d_tensor dim sort: i32 and f32 are not !d_tensor.nat dims.
builtin.module {
  %i = "arith.constant"() <{value = 4 : i32}> : () -> i32
  %f = "test.f"() : () -> f32
  %bad0 = "test.bad0"() : () -> !d_tensor.tensor<[%i], f32>
  %bad1 = "test.bad1"() : () -> !d_tensor.tensor<[%f], f32>
}

// VERIFY: shape SSA parameter must have type !d_tensor.nat or !d_tensor.posnat, got i32

// -----

// Invalid d_tensor element types: tensor and !d_tensor.nat are not valid scalar elems.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %bad0 = "test.bad0"() : () -> !d_tensor.tensor<[%m], tensor<1xf32>>
  %bad1 = "test.bad1"() : () -> !d_tensor.tensor<[%m], !d_tensor.nat>
}

// VERIFY: invalid d_tensor element type

// -----

// Zero-rank policy: currently allowed.
builtin.module {
  %s = "d_tensor.empty"() : () -> !d_tensor.tensor<[], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.empty"() : () -> !d_tensor.tensor<[], f32>
// VERIFY: }

// -----

// Invalid d_tensor.fill element mismatch.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %f = "test.f"() : () -> f32
  %bad = "d_tensor.fill"(%f) : (f32) -> !d_tensor.tensor<[%m], i32>
}

// VERIFY: d_tensor.fill: expected fill value type

// -----

// Invalid d_tensor.dim axis bounds: -1 and rank.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %dneg = "d_tensor.dim"(%a) <{axis = -1 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// VERIFY: d_tensor.dim: axis -1 out of bounds for rank 2

// -----

builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %drank = "d_tensor.dim"(%a) <{axis = 2 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// VERIFY: d_tensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid d_tensor.cast changes dims.
builtin.module {
  %m0 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %m1 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m0], f32>
  %c = "d_tensor.cast"(%a)
    : (!d_tensor.tensor<[%m0], f32>) -> !d_tensor.tensor<[%m1], f32>
}

// VERIFY: d_tensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid d_tensor.add declared result mismatch.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %n0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %n1 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n0], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m, %n0], f32>
  %bad = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%m, %n0], f32>, !d_tensor.tensor<[%m, %n0], f32>) -> !d_tensor.tensor<[%m, %n1], f32>
}

// VERIFY: d_tensor.add: expected pairwise SSA-identical dims for lhs/result

// -----

// Invalid d_tensor.matmul declared result mismatch.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %k = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %x = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %bad = "d_tensor.matmul"(%a, %b)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%x, %n], f32>
}

// VERIFY: d_tensor.matmul: expected result dims to be outer dims

// -----

// Big semantic: matmul chain.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %k = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
  %p = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %B = "test.B"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %C = "test.C"() : () -> !d_tensor.tensor<[%n, %p], f32>
  %X = "d_tensor.matmul"(%A, %B)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  %Y = "d_tensor.matmul"(%X, %C)
    : (!d_tensor.tensor<[%m, %n], f32>, !d_tensor.tensor<[%n, %p], f32>) -> !d_tensor.tensor<[%m, %p], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
// VERIFY:   %1 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
// VERIFY:   %2 = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
// VERIFY:   %3 = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
// VERIFY:   %4 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %5 = "test.B"() : () -> !d_tensor.tensor<[%1, %2], f32>
// VERIFY:   %6 = "test.C"() : () -> !d_tensor.tensor<[%2, %3], f32>
// VERIFY:   %7 = "d_tensor.matmul"(%4, %5) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// VERIFY:   %8 = "d_tensor.matmul"(%7, %6) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%2, %3], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// VERIFY: }

// -----

// Big semantic: semantically equal but not SSA-identical dims must fail.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %s0 = "d_tensor.nat.add"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %s1 = "d_tensor.nat.add"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%s0], f32>
  %c = "d_tensor.cast"(%a)
    : (!d_tensor.tensor<[%s0], f32>) -> !d_tensor.tensor<[%s1], f32>
}

// VERIFY: d_tensor.cast: expected pairwise SSA-identical dims

// -----

// Big semantic: d_tensor.dim identity value reused in a tensor type.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 6 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 9 : i32}> : () -> !d_tensor.nat
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.const"() <{value = 6 : i32}> : () -> !d_tensor.nat
// VERIFY:   %1 = "d_tensor.nat.const"() <{value = 9 : i32}> : () -> !d_tensor.nat
// VERIFY:   %2 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %3 = "d_tensor.dim"(%2) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// VERIFY:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%3], f32>
// VERIFY: }

// -----

// Big semantic: d_tensor.dim result (!value<...>) reused as a dim and dim'd again.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0, %n], f32>
  %d1 = "d_tensor.dim"(%E) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%d0, %n], f32>) -> !value<%d0>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// VERIFY:   %1 = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
// VERIFY:   %2 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %3 = "d_tensor.dim"(%2) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// VERIFY:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%3, %1], f32>
// VERIFY:   %5 = "d_tensor.dim"(%4) <{axis = 0 : i32}> : (!d_tensor.tensor<[%3, %1], f32>) -> !value<%3>
// VERIFY: }

// -----

// Dominance stress: dim value defined in a non-dominating block and used in a type.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %t = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: ssa-dominance: value Value(!d_tensor.nat) does not dominate its use in op `test.use`

// -----

// Parse forward-reference diagnostic for SSA shape params.
builtin.module {
  %t = "test.bad"() : () -> !d_tensor.vector<%m, f32>
  %m = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
}

// PARSE: ssa-dominance: value Value(!d_tensor.nat) does not dominate its use in op `test.bad`

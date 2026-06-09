// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file --parsing-diagnostics | filecheck %s -DFILE=%s --check-prefix=PARSE

// Valid: nat ops + constructors + elementwise + dim + cast.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
  %k = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %mn = "d_tensor.size.mul"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %s = "d_tensor.size.add"(%m, %k) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size

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
// VERIFY:   %0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// VERIFY:   %1 = "d_tensor.size.constant"() <{value = 8 : i32}> : () -> !d_tensor.size
// VERIFY:   %2 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// VERIFY:   %3 = "d_tensor.size.mul"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// VERIFY:   %4 = "d_tensor.size.add"(%0, %2) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
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

// Valid: size.add may refine to !d_tensor.pos_size if either operand is positive.
builtin.module {
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %p = "d_tensor.size.param"() : () -> !d_tensor.pos_size
  %sum0 = "d_tensor.size.add"(%n, %p) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.pos_size
  %sum1 = "d_tensor.size.add"(%p, %n) : (!d_tensor.pos_size, !d_tensor.size) -> !d_tensor.pos_size
  %a = "test.a"() : () -> !d_tensor.tensor<[%sum0], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%sum1], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// VERIFY:   %1 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
// VERIFY:   %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.pos_size
// VERIFY:   %3 = "d_tensor.size.add"(%1, %0) : (!d_tensor.pos_size, !d_tensor.size) -> !d_tensor.pos_size
// VERIFY:   %4 = "test.a"() : () -> !d_tensor.tensor<[%2], f32>
// VERIFY:   %5 = "test.b"() : () -> !d_tensor.tensor<[%3], f32>
// VERIFY: }

// -----

// Valid: refine_positive consumes a dedicated positive-size proof token.
builtin.module {
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %ok = "arith.constant"() <{value = true}> : () -> i1
  %proof = "d_tensor.size.positive_proof"(%n, %ok) : (!d_tensor.size, i1) -> !d_tensor.positive_size_proof
  %p = "d_tensor.size.refine_positive"(%n, %proof) : (!d_tensor.size, !d_tensor.positive_size_proof) -> !d_tensor.pos_size
  %t = "test.t"() : () -> !d_tensor.tensor<[%p], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// VERIFY:   %1 = "arith.constant"() <{value = true}> : () -> i1
// VERIFY:   %2 = "d_tensor.size.positive_proof"(%0, %1) : (!d_tensor.size, i1) -> !d_tensor.positive_size_proof
// VERIFY:   %3 = "d_tensor.size.refine_positive"(%0, %2) : (!d_tensor.size, !d_tensor.positive_size_proof) -> !d_tensor.pos_size
// VERIFY:   %4 = "test.t"() : () -> !d_tensor.tensor<[%3], f32>
// VERIFY: }

// -----

// Invalid: nat.const literal must be >= 0.
builtin.module {
  %n = "d_tensor.size.constant"() <{value = -1 : i32}> : () -> !d_tensor.size
}

// VERIFY: d_tensor.size.constant: expected non-negative literal

// -----

// Invalid: nat.const missing required `value` attribute.
builtin.module {
  %n = "d_tensor.size.constant"() : () -> !d_tensor.size
}

// VERIFY: Missing required property "value"

// -----

// Invalid: size.add wrong operand type.
builtin.module {
  %x = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %m = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %r = "d_tensor.size.add"(%x, %m) : (i32, !d_tensor.size) -> !d_tensor.size
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// VERIFY:   %1 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// VERIFY:   %2 = "d_tensor.size.add"(%0, %1) : (i32, !d_tensor.size) -> !d_tensor.size
// VERIFY: }

// -----

// Invalid: size.mul wrong operand arity.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %r = "d_tensor.size.mul"(%m) : (!d_tensor.size) -> !d_tensor.size
}

// VERIFY: Expected 2 operands, got 1.

// -----

// Valid: size.import is an explicit boundary obligation from runtime index to shape witness.
builtin.module {
  %p = "arith.constant"() <{value = 12 : index}> : () -> index
  %nat = "d_tensor.size.import"(%p) : (index) -> !d_tensor.size
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 12 : index}> : () -> index
// VERIFY:   %1 = "d_tensor.size.import"(%0) : (index) -> !d_tensor.size
// VERIFY: }

// -----

// Invalid d_tensor dim sort: i32 and f32 are not !d_tensor.size dims.
builtin.module {
  %i = "arith.constant"() <{value = 4 : i32}> : () -> i32
  %f = "test.f"() : () -> f32
  %bad0 = "test.bad0"() : () -> !d_tensor.tensor<[%i], f32>
  %bad1 = "test.bad1"() : () -> !d_tensor.tensor<[%f], f32>
}

// VERIFY: shape SSA parameter must have type !d_tensor.size or !d_tensor.pos_size, got i32

// -----

// Invalid d_tensor element types: tensor and !d_tensor.size are not valid scalar elems.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %bad0 = "test.bad0"() : () -> !d_tensor.tensor<[%m], tensor<1xf32>>
  %bad1 = "test.bad1"() : () -> !d_tensor.tensor<[%m], !d_tensor.size>
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
  %m = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %f = "test.f"() : () -> f32
  %bad = "d_tensor.fill"(%f) : (f32) -> !d_tensor.tensor<[%m], i32>
}

// VERIFY: d_tensor.fill: expected fill value type

// -----

// Invalid d_tensor.dim axis bounds: -1 and rank.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %dneg = "d_tensor.dim"(%a) <{axis = -1 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// VERIFY: d_tensor.dim: axis -1 out of bounds for rank 2

// -----

builtin.module {
  %m = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %drank = "d_tensor.dim"(%a) <{axis = 2 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// VERIFY: d_tensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid d_tensor.cast changes dims.
builtin.module {
  %m0 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %m1 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m0], f32>
  %c = "d_tensor.cast"(%a)
    : (!d_tensor.tensor<[%m0], f32>) -> !d_tensor.tensor<[%m1], f32>
}

// VERIFY: d_tensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid d_tensor.add declared result mismatch.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %n0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %n1 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n0], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m, %n0], f32>
  %bad = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%m, %n0], f32>, !d_tensor.tensor<[%m, %n0], f32>) -> !d_tensor.tensor<[%m, %n1], f32>
}

// VERIFY: d_tensor.add: expected pairwise SSA-identical dims for lhs/result

// -----

// Invalid d_tensor.matmul declared result mismatch.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %k = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %x = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %bad = "d_tensor.matmul"(%a, %b)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%x, %n], f32>
}

// VERIFY: d_tensor.matmul: expected result dims to be outer dims

// -----

// Big semantic: matmul chain.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %k = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
  %p = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %B = "test.B"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %C = "test.C"() : () -> !d_tensor.tensor<[%n, %p], f32>
  %X = "d_tensor.matmul"(%A, %B)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  %Y = "d_tensor.matmul"(%X, %C)
    : (!d_tensor.tensor<[%m, %n], f32>, !d_tensor.tensor<[%n, %p], f32>) -> !d_tensor.tensor<[%m, %p], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
// VERIFY:   %1 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// VERIFY:   %2 = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
// VERIFY:   %3 = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
// VERIFY:   %4 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %5 = "test.B"() : () -> !d_tensor.tensor<[%1, %2], f32>
// VERIFY:   %6 = "test.C"() : () -> !d_tensor.tensor<[%2, %3], f32>
// VERIFY:   %7 = "d_tensor.matmul"(%4, %5) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// VERIFY:   %8 = "d_tensor.matmul"(%7, %6) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%2, %3], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// VERIFY: }

// -----

// Big semantic: semantically equal but not SSA-identical dims must fail.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
  %s0 = "d_tensor.size.add"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %s1 = "d_tensor.size.add"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%s0], f32>
  %c = "d_tensor.cast"(%a)
    : (!d_tensor.tensor<[%s0], f32>) -> !d_tensor.tensor<[%s1], f32>
}

// VERIFY: d_tensor.cast: expected pairwise SSA-identical dims

// -----

// Big semantic: d_tensor.dim identity value reused in a tensor type.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 6 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 9 : i32}> : () -> !d_tensor.size
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.size.constant"() <{value = 6 : i32}> : () -> !d_tensor.size
// VERIFY:   %1 = "d_tensor.size.constant"() <{value = 9 : i32}> : () -> !d_tensor.size
// VERIFY:   %2 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %3 = "d_tensor.dim"(%2) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// VERIFY:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%3], f32>
// VERIFY: }

// -----

// Big semantic: d_tensor.dim result (!value<...>) reused as a dim and dim'd again.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0, %n], f32>
  %d1 = "d_tensor.dim"(%E) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%d0, %n], f32>) -> !value<%d0>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// VERIFY:   %1 = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
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
    %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %t = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: ssa-dominance: value Value(!d_tensor.size) does not dominate its use in op `test.use`

// -----

// Parse forward-reference diagnostic for SSA shape params.
builtin.module {
  %t = "test.bad"() : () -> !d_tensor.vector<%m, f32>
  %m = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
}

// PARSE: ssa-dominance: value Value(!d_tensor.size) does not dominate its use in op `test.bad`

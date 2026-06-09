// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file --parsing-diagnostics | filecheck %s -DFILE=%s --check-prefix=PARSE

// Valid: index ops + constructors + elementwise + dim + cast.
builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 8 : index}> : () -> index
  %k = "arith.constant"() <{value = 3 : index}> : () -> index
  %mn = "arith.muli"(%m, %n) : (index, index) -> index
  %s = "arith.addi"(%m, %k) : (index, index) -> index

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
// VERIFY:   %0 = "arith.constant"() <{value = 4 : index}> : () -> index
// VERIFY:   %1 = "arith.constant"() <{value = 8 : index}> : () -> index
// VERIFY:   %2 = "arith.constant"() <{value = 3 : index}> : () -> index
// VERIFY:   %3 = "arith.muli"(%0, %1) {{.*}} : (index, index) -> index
// VERIFY:   %4 = "arith.addi"(%0, %2) {{.*}} : (index, index) -> index
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

// Valid: arith.addi index results may be used directly as dimensions.
builtin.module {
  %n = "test.index"() : () -> index
  %p = "test.index"() : () -> index
  %sum0 = "arith.addi"(%n, %p) : (index, index) -> index
  %sum1 = "arith.addi"(%p, %n) : (index, index) -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%sum0], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%sum1], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.index"() : () -> index
// VERIFY:   %1 = "test.index"() : () -> index
// VERIFY:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// VERIFY:   %3 = "arith.addi"(%1, %0) {{.*}} : (index, index) -> index
// VERIFY:   %4 = "test.a"() : () -> !d_tensor.tensor<[%2], f32>
// VERIFY:   %5 = "test.b"() : () -> !d_tensor.tensor<[%3], f32>
// VERIFY: }

// -----

// Valid: static integer dimensions are accepted directly.
builtin.module {
  %t = "test.t"() : () -> !d_tensor.tensor<[4, 8], f32>
  %v = "test.v"() : () -> !d_tensor.vector<4, f32>
  %m = "test.m"() : () -> !d_tensor.matrix<4, 8, f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.t"() : () -> !d_tensor.tensor<[4, 8], f32>
// VERIFY:   %1 = "test.v"() : () -> !d_tensor.vector<4, f32>
// VERIFY:   %2 = "test.m"() : () -> !d_tensor.matrix<4, 8, f32>
// VERIFY: }

// -----

// Invalid: arith.constant missing required `value` attribute.
builtin.module {
  %n = "arith.constant"() : () -> index
}

// VERIFY: Missing required property "value"

// -----

// Invalid: arith.addi wrong operand type.
builtin.module {
  %x = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %m = "arith.constant"() <{value = 3 : index}> : () -> index
  %r = "arith.addi"(%x, %m) : (i32, index) -> index
}

// VERIFY: All parameters of TypeConstraint must be of the same type in operation arith.addi

// -----

// Invalid: arith.muli wrong operand arity.
builtin.module {
  %m = "arith.constant"() <{value = 2 : index}> : () -> index
  %r = "arith.muli"(%m) : (index) -> index
}

// VERIFY: Expected 2 operands, got 1.

// -----

// Invalid d_tensor dim sort: i32 and f32 are not index dims.
builtin.module {
  %i = "arith.constant"() <{value = 4 : i32}> : () -> i32
  %f = "test.f"() : () -> f32
  %bad0 = "test.bad0"() : () -> !d_tensor.tensor<[%i], f32>
  %bad1 = "test.bad1"() : () -> !d_tensor.tensor<[%f], f32>
}

// VERIFY: shape SSA parameter must have type index, got i32

// -----

// Invalid d_tensor element types: tensor and index are not valid scalar elems.
builtin.module {
  %m = "arith.constant"() <{value = 5 : index}> : () -> index
  %bad0 = "test.bad0"() : () -> !d_tensor.tensor<[%m], tensor<1xf32>>
  %bad1 = "test.bad1"() : () -> !d_tensor.tensor<[%m], index>
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
  %m = "arith.constant"() <{value = 2 : index}> : () -> index
  %f = "test.f"() : () -> f32
  %bad = "d_tensor.fill"(%f) : (f32) -> !d_tensor.tensor<[%m], i32>
}

// VERIFY: d_tensor.fill: expected fill value type

// -----

// Invalid d_tensor.dim axis bounds: -1 and rank.
builtin.module {
  %m = "arith.constant"() <{value = 2 : index}> : () -> index
  %n = "arith.constant"() <{value = 5 : index}> : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %dneg = "d_tensor.dim"(%a) <{axis = -1 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// VERIFY: d_tensor.dim: axis -1 out of bounds for rank 2

// -----

builtin.module {
  %m = "arith.constant"() <{value = 2 : index}> : () -> index
  %n = "arith.constant"() <{value = 5 : index}> : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %drank = "d_tensor.dim"(%a) <{axis = 2 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// VERIFY: d_tensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid d_tensor.cast changes dims.
builtin.module {
  %m0 = "arith.constant"() <{value = 2 : index}> : () -> index
  %m1 = "arith.constant"() <{value = 2 : index}> : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m0], f32>
  %c = "d_tensor.cast"(%a)
    : (!d_tensor.tensor<[%m0], f32>) -> !d_tensor.tensor<[%m1], f32>
}

// VERIFY: d_tensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid d_tensor.add declared result mismatch.
builtin.module {
  %m = "arith.constant"() <{value = 3 : index}> : () -> index
  %n0 = "arith.constant"() <{value = 4 : index}> : () -> index
  %n1 = "arith.constant"() <{value = 4 : index}> : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n0], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%m, %n0], f32>
  %bad = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%m, %n0], f32>, !d_tensor.tensor<[%m, %n0], f32>) -> !d_tensor.tensor<[%m, %n1], f32>
}

// VERIFY: d_tensor.add: expected pairwise SSA-identical dims for lhs/result

// -----

// Invalid d_tensor.matmul declared result mismatch.
builtin.module {
  %m = "arith.constant"() <{value = 2 : index}> : () -> index
  %k = "arith.constant"() <{value = 3 : index}> : () -> index
  %n = "arith.constant"() <{value = 5 : index}> : () -> index
  %x = "arith.constant"() <{value = 5 : index}> : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %b = "test.b"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %bad = "d_tensor.matmul"(%a, %b)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%x, %n], f32>
}

// VERIFY: d_tensor.matmul: expected result dims to be outer dims

// -----

// Big semantic: matmul chain.
builtin.module {
  %m = "arith.constant"() <{value = 2 : index}> : () -> index
  %k = "arith.constant"() <{value = 3 : index}> : () -> index
  %n = "arith.constant"() <{value = 5 : index}> : () -> index
  %p = "arith.constant"() <{value = 7 : index}> : () -> index
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %k], f32>
  %B = "test.B"() : () -> !d_tensor.tensor<[%k, %n], f32>
  %C = "test.C"() : () -> !d_tensor.tensor<[%n, %p], f32>
  %X = "d_tensor.matmul"(%A, %B)
    : (!d_tensor.tensor<[%m, %k], f32>, !d_tensor.tensor<[%k, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  %Y = "d_tensor.matmul"(%X, %C)
    : (!d_tensor.tensor<[%m, %n], f32>, !d_tensor.tensor<[%n, %p], f32>) -> !d_tensor.tensor<[%m, %p], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 2 : index}> : () -> index
// VERIFY:   %1 = "arith.constant"() <{value = 3 : index}> : () -> index
// VERIFY:   %2 = "arith.constant"() <{value = 5 : index}> : () -> index
// VERIFY:   %3 = "arith.constant"() <{value = 7 : index}> : () -> index
// VERIFY:   %4 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %5 = "test.B"() : () -> !d_tensor.tensor<[%1, %2], f32>
// VERIFY:   %6 = "test.C"() : () -> !d_tensor.tensor<[%2, %3], f32>
// VERIFY:   %7 = "d_tensor.matmul"(%4, %5) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%1, %2], f32>) -> !d_tensor.tensor<[%0, %2], f32>
// VERIFY:   %8 = "d_tensor.matmul"(%7, %6) : (!d_tensor.tensor<[%0, %2], f32>, !d_tensor.tensor<[%2, %3], f32>) -> !d_tensor.tensor<[%0, %3], f32>
// VERIFY: }

// -----

// Big semantic: semantically equal but not SSA-identical dims must fail.
builtin.module {
  %m = "arith.constant"() <{value = 2 : index}> : () -> index
  %n = "arith.constant"() <{value = 3 : index}> : () -> index
  %s0 = "arith.addi"(%m, %n) : (index, index) -> index
  %s1 = "arith.addi"(%m, %n) : (index, index) -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%s0], f32>
  %c = "d_tensor.cast"(%a)
    : (!d_tensor.tensor<[%s0], f32>) -> !d_tensor.tensor<[%s1], f32>
}

// VERIFY: d_tensor.cast: expected pairwise SSA-identical dims

// -----

// Big semantic: d_tensor.dim identity value reused in a tensor type.
builtin.module {
  %m = "arith.constant"() <{value = 6 : index}> : () -> index
  %n = "arith.constant"() <{value = 9 : index}> : () -> index
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0], f32>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 6 : index}> : () -> index
// VERIFY:   %1 = "arith.constant"() <{value = 9 : index}> : () -> index
// VERIFY:   %2 = "test.A"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   %3 = "d_tensor.dim"(%2) <{axis = 0 : i32}> : (!d_tensor.tensor<[%0, %1], f32>) -> !value<%0>
// VERIFY:   %4 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%3], f32>
// VERIFY: }

// -----

// Big semantic: d_tensor.dim result (!value<...>) reused as a dim and dim'd again.
builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 7 : index}> : () -> index
  %A = "test.A"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %d0 = "d_tensor.dim"(%A) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "d_tensor.empty"() : () -> !d_tensor.tensor<[%d0, %n], f32>
  %d1 = "d_tensor.dim"(%E) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%d0, %n], f32>) -> !value<%d0>
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "arith.constant"() <{value = 4 : index}> : () -> index
// VERIFY:   %1 = "arith.constant"() <{value = 7 : index}> : () -> index
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
    %m = "arith.constant"() <{value = 4 : index}> : () -> index
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %t = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: ssa-dominance: value Value(index) does not dominate its use in op `test.use`

// -----

// Parse forward-reference diagnostic for SSA shape params.
builtin.module {
  %t = "test.bad"() : () -> !d_tensor.vector<%m, f32>
  %m = "arith.constant"() <{value = 7 : index}> : () -> index
}

// PARSE: ssa-dominance: value Value(index) does not dominate its use in op `test.bad`

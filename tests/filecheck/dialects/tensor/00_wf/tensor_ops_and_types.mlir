// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file --parsing-diagnostics | filecheck %s -DFILE=%s --check-prefix=PARSE

// Valid: nat ops + constructors + elementwise + dim + cast.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %k = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %mn = "dtensor.nat.mul"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s = "dtensor.nat.add"(%m, %k) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %zero = "test.zero"() : () -> f32
  %e = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %f = "dtensor.fill"(%zero) : (f32) -> !dtensor.tensor<[%m, %n], f32>
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m, %n], f32>
  %sum = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %prod = "dtensor.mul"(%a, %b)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %d1 = "dtensor.dim"(%a) <{axis = 1 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %c = "dtensor.cast"(%a)
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "dtensor.nat.const"
// VERIFY: "dtensor.nat.mul"
// VERIFY: "dtensor.nat.add"
// VERIFY: "dtensor.empty"
// VERIFY: "dtensor.fill"
// VERIFY: "dtensor.add"
// VERIFY: "dtensor.mul"
// VERIFY: "dtensor.dim"
// VERIFY: "dtensor.cast"
// VERIFY: }

// -----

// Invalid: nat.const literal must be >= 0.
builtin.module {
  %n = "dtensor.nat.const"() <{value = -1 : i32}> : () -> !dtensor.nat
}

// VERIFY: dtensor.nat.const: expected non-negative literal

// -----

// Invalid: nat.const missing required `value` attribute.
builtin.module {
  %n = "dtensor.nat.const"() : () -> !dtensor.nat
}

// VERIFY: Missing required property "value"

// -----

// Invalid: nat.add wrong operand type.
builtin.module {
  %x = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %m = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %r = "dtensor.nat.add"(%x, %m) : (i32, !dtensor.nat) -> !dtensor.nat
}

// VERIFY: dtensor.nat.add
// VERIFY: !dtensor.nat

// -----

// Invalid: nat.mul wrong operand arity.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %r = "dtensor.nat.mul"(%m) : (!dtensor.nat) -> !dtensor.nat
}

// VERIFY: Expected 2 operands, got 1.

// -----

// Invalid dtensor dim sort: i32 and f32 are not !dtensor.nat dims.
builtin.module {
  %i = "arith.constant"() <{value = 4 : i32}> : () -> i32
  %f = "test.f"() : () -> f32
  %bad0 = "test.bad0"() : () -> !dtensor.tensor<[%i], f32>
  %bad1 = "test.bad1"() : () -> !dtensor.tensor<[%f], f32>
}

// VERIFY: shape SSA parameter must have type !dtensor.nat, got i32

// -----

// Invalid dtensor element types: tensor and !dtensor.nat are not valid scalar elems.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %bad0 = "test.bad0"() : () -> !dtensor.tensor<[%m], tensor<1xf32>>
  %bad1 = "test.bad1"() : () -> !dtensor.tensor<[%m], !dtensor.nat>
}

// VERIFY: invalid dtensor element type

// -----

// Zero-rank policy: currently allowed.
builtin.module {
  %s = "dtensor.empty"() : () -> !dtensor.tensor<[], f32>
}

// VERIFY: !dtensor.tensor<[], f32>

// -----

// Invalid dtensor.fill element mismatch.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %f = "test.f"() : () -> f32
  %bad = "dtensor.fill"(%f) : (f32) -> !dtensor.tensor<[%m], i32>
}

// VERIFY: dtensor.fill: expected fill value type

// -----

// Invalid dtensor.dim axis bounds: -1 and rank.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %dneg = "dtensor.dim"(%a) <{axis = -1 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// VERIFY: dtensor.dim: axis -1 out of bounds for rank 2

// -----

builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %drank = "dtensor.dim"(%a) <{axis = 2 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// VERIFY: dtensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid dtensor.cast changes dims.
builtin.module {
  %m0 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %m1 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m0], f32>
  %c = "dtensor.cast"(%a)
    : (!dtensor.tensor<[%m0], f32>) -> !dtensor.tensor<[%m1], f32>
}

// VERIFY: dtensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid dtensor.add declared result mismatch.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %n0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n1 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n0], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m, %n0], f32>
  %bad = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m, %n0], f32>, !dtensor.tensor<[%m, %n0], f32>) -> !dtensor.tensor<[%m, %n1], f32>
}

// VERIFY: dtensor.add: expected pairwise SSA-identical dims for lhs/result

// -----

// Invalid dtensor.matmul declared result mismatch.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %k = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %x = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %k], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%k, %n], f32>
  %bad = "dtensor.matmul"(%a, %b)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%x, %n], f32>
}

// VERIFY: dtensor.matmul: expected result dims to be outer dims

// -----

// Big semantic: matmul chain.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %k = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %p = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
  %A = "test.A"() : () -> !dtensor.tensor<[%m, %k], f32>
  %B = "test.B"() : () -> !dtensor.tensor<[%k, %n], f32>
  %C = "test.C"() : () -> !dtensor.tensor<[%n, %p], f32>
  %X = "dtensor.matmul"(%A, %B)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %Y = "dtensor.matmul"(%X, %C)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%n, %p], f32>) -> !dtensor.tensor<[%m, %p], f32>
}

// VERIFY: "dtensor.matmul"
// VERIFY: -> !dtensor.tensor<[%0, %2], f32>
// VERIFY: "dtensor.matmul"
// VERIFY: -> !dtensor.tensor<[%0, %3], f32>

// -----

// Big semantic: semantically equal but not SSA-identical dims must fail.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %s0 = "dtensor.nat.add"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s1 = "dtensor.nat.add"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%s0], f32>
  %c = "dtensor.cast"(%a)
    : (!dtensor.tensor<[%s0], f32>) -> !dtensor.tensor<[%s1], f32>
}

// VERIFY: dtensor.cast: expected pairwise SSA-identical dims

// -----

// Big semantic: dtensor.dim identity value reused in a tensor type.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 6 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 9 : i32}> : () -> !dtensor.nat
  %A = "test.A"() : () -> !dtensor.tensor<[%m, %n], f32>
  %d0 = "dtensor.dim"(%A) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "dtensor.empty"() : () -> !dtensor.tensor<[%d0], f32>
}

// VERIFY: "dtensor.dim"
// VERIFY: -> !value<%
// VERIFY: "dtensor.empty"() : () -> !dtensor.tensor<[%3], f32>

// -----

// Big semantic: dtensor.dim result (!value<...>) reused as a dim and dim'd again.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
  %A = "test.A"() : () -> !dtensor.tensor<[%m, %n], f32>
  %d0 = "dtensor.dim"(%A) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %E = "dtensor.empty"() : () -> !dtensor.tensor<[%d0, %n], f32>
  %d1 = "dtensor.dim"(%E) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%d0, %n], f32>) -> !value<%d0>
}

// VERIFY: "dtensor.dim"
// VERIFY: -> !value<%
// VERIFY: "dtensor.empty"() : () -> !dtensor.tensor<[
// VERIFY: "dtensor.dim"
// VERIFY: -> !value<%

// -----

// Dominance stress: dim value defined in a non-dominating block and used in a type.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %t = "test.use"() : () -> !dtensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: ssa-dominance: value Value
// VERIFY: does not dominate its use in op `test.use`

// -----

// Parse forward-reference diagnostic for SSA shape params.
builtin.module {
  %t = "test.bad"() : () -> !tensor.vector<%m, f32>
  %m = "dtensor.nat.const"() <{value = 7 : i32}> : () -> !dtensor.nat
}

// PARSE: Parse error at [[FILE]]
// PARSE: Type tensor.vector is not defined in any supported Dialect.

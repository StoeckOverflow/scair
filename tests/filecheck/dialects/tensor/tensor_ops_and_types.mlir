// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file --parsing-diagnostics | filecheck %s -DFILE=%s --check-prefix=PARSE

// Valid: nat ops + constructors + elementwise + dim + cast.
builtin.module {
  %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 8 : i32}> : () -> !tensor.nat
  %k = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %mn = "tensor.nat.mul"(%m, %n) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %s = "tensor.nat.add"(%m, %k) : (!tensor.nat, !tensor.nat) -> !tensor.nat

  %zero = "test.zero"() : () -> f32
  %e = "tensor.empty"() : () -> !tensor.tensor<[%m, %n], f32>
  %f = "tensor.fill"(%zero) : (f32) -> !tensor.tensor<[%m, %n], f32>
  %a = "test.a"() : () -> !tensor.tensor<[%m, %n], f32>
  %b = "test.b"() : () -> !tensor.tensor<[%m, %n], f32>
  %sum = "tensor.add"(%a, %b)
    : (!tensor.tensor<[%m, %n], f32>, !tensor.tensor<[%m, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
  %prod = "tensor.mul"(%a, %b)
    : (!tensor.tensor<[%m, %n], f32>, !tensor.tensor<[%m, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
  %d1 = "tensor.dim"(%a) <{axis = 1 : i32}>
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.nat
  %c = "tensor.cast"(%a)
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "tensor.nat.const"
// VERIFY: "tensor.nat.mul"
// VERIFY: "tensor.nat.add"
// VERIFY: "tensor.empty"
// VERIFY: "tensor.fill"
// VERIFY: "tensor.add"
// VERIFY: "tensor.mul"
// VERIFY: "tensor.dim"
// VERIFY: "tensor.cast"
// VERIFY: }

// -----

// Invalid: nat.const literal must be >= 0.
builtin.module {
  %n = "tensor.nat.const"() <{value = -1 : i32}> : () -> !tensor.nat
}

// VERIFY: tensor.nat.const: expected non-negative literal

// -----

// Invalid: nat.const missing required `value` attribute.
builtin.module {
  %n = "tensor.nat.const"() : () -> !tensor.nat
}

// VERIFY: Missing required property "value"

// -----

// Invalid: nat.add wrong operand type.
builtin.module {
  %x = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %m = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %r = "tensor.nat.add"(%x, %m) : (i32, !tensor.nat) -> !tensor.nat
}

// VERIFY: tensor.nat.add
// VERIFY: !tensor.nat

// -----

// Invalid: nat.mul wrong operand arity.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %r = "tensor.nat.mul"(%m) : (!tensor.nat) -> !tensor.nat
}

// VERIFY: Expected 2 operands, got 1.

// -----

// Invalid tensor dim sort: i32 and f32 are not !tensor.nat dims.
builtin.module {
  %i = "arith.constant"() <{value = 4 : i32}> : () -> i32
  %f = "test.f"() : () -> f32
  %bad0 = "test.bad0"() : () -> !tensor.tensor<[%i], f32>
  %bad1 = "test.bad1"() : () -> !tensor.tensor<[%f], f32>
}

// VERIFY: shape SSA parameter must have type !tensor.nat, got i32

// -----

// Invalid tensor element types: tensor and !tensor.nat are not valid scalar elems.
builtin.module {
  %m = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %bad0 = "test.bad0"() : () -> !tensor.tensor<[%m], tensor<1xf32>>
  %bad1 = "test.bad1"() : () -> !tensor.tensor<[%m], !tensor.nat>
}

// VERIFY: invalid tensor element type

// -----

// Zero-rank policy: currently allowed.
builtin.module {
  %s = "tensor.empty"() : () -> !tensor.tensor<[], f32>
}

// VERIFY: !tensor.tensor<[], f32>

// -----

// Invalid tensor.fill element mismatch.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %f = "test.f"() : () -> f32
  %bad = "tensor.fill"(%f) : (f32) -> !tensor.tensor<[%m], i32>
}

// VERIFY: tensor.fill: expected fill value type

// -----

// Invalid tensor.dim axis bounds: -1 and rank.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m, %n], f32>
  %dneg = "tensor.dim"(%a) <{axis = -1 : i32}>
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.nat
}

// VERIFY: tensor.dim: axis -1 out of bounds for rank 2

// -----

builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m, %n], f32>
  %drank = "tensor.dim"(%a) <{axis = 2 : i32}>
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.nat
}

// VERIFY: tensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid tensor.cast changes dims.
builtin.module {
  %m0 = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %m1 = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m0], f32>
  %c = "tensor.cast"(%a)
    : (!tensor.tensor<[%m0], f32>) -> !tensor.tensor<[%m1], f32>
}

// VERIFY: tensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid tensor.add declared result mismatch.
builtin.module {
  %m = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %n0 = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %n1 = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m, %n0], f32>
  %b = "test.b"() : () -> !tensor.tensor<[%m, %n0], f32>
  %bad = "tensor.add"(%a, %b)
    : (!tensor.tensor<[%m, %n0], f32>, !tensor.tensor<[%m, %n0], f32>) -> !tensor.tensor<[%m, %n1], f32>
}

// VERIFY: tensor.add: expected pairwise SSA-identical dims for lhs/result

// -----

// Invalid tensor.matmul declared result mismatch.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %k = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %x = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m, %k], f32>
  %b = "test.b"() : () -> !tensor.tensor<[%k, %n], f32>
  %bad = "tensor.matmul"(%a, %b)
    : (!tensor.tensor<[%m, %k], f32>, !tensor.tensor<[%k, %n], f32>) -> !tensor.tensor<[%x, %n], f32>
}

// VERIFY: tensor.matmul: expected result dims to be outer dims

// -----

// Big semantic: matmul chain.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %k = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %p = "tensor.nat.const"() <{value = 7 : i32}> : () -> !tensor.nat
  %A = "test.A"() : () -> !tensor.tensor<[%m, %k], f32>
  %B = "test.B"() : () -> !tensor.tensor<[%k, %n], f32>
  %C = "test.C"() : () -> !tensor.tensor<[%n, %p], f32>
  %X = "tensor.matmul"(%A, %B)
    : (!tensor.tensor<[%m, %k], f32>, !tensor.tensor<[%k, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
  %Y = "tensor.matmul"(%X, %C)
    : (!tensor.tensor<[%m, %n], f32>, !tensor.tensor<[%n, %p], f32>) -> !tensor.tensor<[%m, %p], f32>
}

// VERIFY: "tensor.matmul"
// VERIFY: -> !tensor.tensor<[%0, %2], f32>
// VERIFY: "tensor.matmul"
// VERIFY: -> !tensor.tensor<[%0, %3], f32>

// -----

// Big semantic: semantically equal but not SSA-identical dims must fail.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %s0 = "tensor.nat.add"(%m, %n) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %s1 = "tensor.nat.add"(%m, %n) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%s0], f32>
  %c = "tensor.cast"(%a)
    : (!tensor.tensor<[%s0], f32>) -> !tensor.tensor<[%s1], f32>
}

// VERIFY: tensor.cast: expected pairwise SSA-identical dims

// -----

// Big semantic: tensor.dim identity value reused in a tensor type.
builtin.module {
  %m = "tensor.nat.const"() <{value = 6 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 9 : i32}> : () -> !tensor.nat
  %A = "test.A"() : () -> !tensor.tensor<[%m, %n], f32>
  %d0 = "tensor.dim"(%A) <{axis = 0 : i32}>
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.nat
  %E = "tensor.empty"() : () -> !tensor.tensor<[%d0], f32>
}

// VERIFY: "tensor.dim"
// VERIFY: -> !tensor.nat
// VERIFY: "tensor.empty"() : () -> !tensor.tensor<[%3], f32>

// -----

// Dominance stress: dim value defined in a non-dominating block and used in a type.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %t = "test.use"() : () -> !tensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: ssa-dominance: value Value
// VERIFY: does not dominate its use in op `test.use`

// -----

// Parse forward-reference diagnostic for SSA shape params.
builtin.module {
  %t = "test.bad"() : () -> !tensor.vector<%m, f32>
  %m = "tensor.nat.const"() <{value = 7 : i32}> : () -> !tensor.nat
}

// PARSE: Parse error at [[FILE]]
// PARSE: Value %m must be defined before use in this context.

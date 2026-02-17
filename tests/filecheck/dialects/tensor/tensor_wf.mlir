// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=PARSE

// Valid: core tensor SSA-shape ops.
builtin.module {
  %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 8 : i32}> : () -> !tensor.nat
  %k = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %s = "tensor.nat.add"(%m, %k) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %p = "tensor.nat.mul"(%s, %n) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %z = "test.zero"() : () -> f32
  %a = "tensor.fill"(%z) : (f32) -> !tensor.tensor<[%m, %k], f32>
  %b = "tensor.empty"() : () -> !tensor.tensor<[%k, %n], f32>
  %x = "tensor.matmul"(%a, %b)
    : (!tensor.tensor<[%m, %k], f32>, !tensor.tensor<[%k, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
  %d0 = "tensor.dim"(%x) <{axis = 0 : i32}>
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.nat
  %v0 = "test.v0"() : () -> !tensor.vector<%m, f32>
  %m0 = "test.m0"() : () -> !tensor.matrix<%m, %n, f32>
  %c = "tensor.cast"(%x)
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "tensor.nat.add"
// VERIFY: "tensor.nat.mul"
// VERIFY: "tensor.matmul"
// VERIFY: "tensor.dim"
// VERIFY: "tensor.cast"
// VERIFY: }

// -----

// Invalid: nat.const literal must be non-negative.
builtin.module {
  %n = "tensor.nat.const"() <{value = -1 : i32}> : () -> !tensor.nat
}

// VERIFY: tensor.nat.const: expected non-negative literal

// -----

// Invalid: dim sort is not !tensor.nat.
builtin.module {
  %i = "arith.constant"() <{value = 1 : i32}> : () -> i32
  %t = "test.bad"() : () -> !tensor.tensor<[%i], f32>
}

// VERIFY: shape SSA parameter must have type !tensor.nat, got i32

// -----

// Invalid: element type is non-scalar.
builtin.module {
  %m = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %t = "test.bad"() : () -> !tensor.tensor<[%m], tensor<1xf32>>
}

// VERIFY: invalid tensor element type

// -----

// Invalid: tensor.dim axis out-of-bounds.
builtin.module {
  %m = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m, %n], f32>
  %d = "tensor.dim"(%a) <{axis = 2 : i32}>
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.nat
}

// VERIFY: tensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid: tensor.add dims are not SSA-identical.
builtin.module {
  %m0 = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %m1 = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m0], f32>
  %b = "test.b"() : () -> !tensor.tensor<[%m1], f32>
  %x = "tensor.add"(%a, %b)
    : (!tensor.tensor<[%m0], f32>, !tensor.tensor<[%m1], f32>) -> !tensor.tensor<[%m0], f32>
}

// VERIFY: tensor.add: expected pairwise SSA-identical dims for lhs/rhs

// -----

// Invalid: tensor.matmul inner dim mismatch.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %k0 = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %k1 = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m, %k0], f32>
  %b = "test.b"() : () -> !tensor.tensor<[%k1, %n], f32>
  %x = "tensor.matmul"(%a, %b)
    : (!tensor.tensor<[%m, %k0], f32>, !tensor.tensor<[%k1, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
}

// VERIFY: tensor.matmul: expected SSA-identical inner dims

// -----

// Invalid: strict cast forbids semantically-equal but non-identical dims.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %z = "tensor.nat.const"() <{value = 0 : i32}> : () -> !tensor.nat
  %s0 = "tensor.nat.add"(%m, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %s1 = "tensor.nat.add"(%m, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%s0], f32>
  %c = "tensor.cast"(%a)
    : (!tensor.tensor<[%s0], f32>) -> !tensor.tensor<[%s1], f32>
}

// VERIFY: tensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid: dominance-in-types violation with non-dominating dim.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %u = "test.use"() : () -> !tensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: ssa-dominance: value Value
// VERIFY: does not dominate its use in op `test.use`

// -----

// Parse: vector arity mismatch.
builtin.module {
  %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %v = "test.bad"() : () -> !tensor.vector<%m, %n, f32>
}

// PARSE: Parse error at [[FILE]]

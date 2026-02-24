// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=PARSE

// Valid: core tensor SSA-shape ops.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %k = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%m, %k) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %p = "dtensor.nat.mul"(%s, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %z = "test.zero"() : () -> f32
  %a = "dtensor.fill"(%z) : (f32) -> !dtensor.tensor<[%m, %k], f32>
  %b = "dtensor.empty"() : () -> !dtensor.tensor<[%k, %n], f32>
  %x = "dtensor.matmul"(%a, %b)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %d0 = "dtensor.dim"(%x) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %v0 = "test.v0"() : () -> !dtensor.vector<%m, f32>
  %m0 = "test.m0"() : () -> !dtensor.matrix<%m, %n, f32>
  %c = "dtensor.cast"(%x)
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "dtensor.nat.add"
// VERIFY: "dtensor.nat.mul"
// VERIFY: "dtensor.matmul"
// VERIFY: "dtensor.dim"
// VERIFY: "dtensor.cast"
// VERIFY: }

// -----

// Invalid: nat.const literal must be non-negative.
builtin.module {
  %n = "dtensor.nat.const"() <{value = -1 : i32}> : () -> !dtensor.nat
}

// VERIFY: dtensor.nat.const: expected non-negative literal

// -----

// Invalid: dim sort is not !dtensor.nat.
builtin.module {
  %i = "arith.constant"() <{value = 1 : i32}> : () -> i32
  %t = "test.bad"() : () -> !dtensor.tensor<[%i], f32>
}

// VERIFY: shape SSA parameter must have type !dtensor.nat, got i32

// -----

// Invalid: element type is non-scalar.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %t = "test.bad"() : () -> !dtensor.tensor<[%m], tensor<1xf32>>
}

// VERIFY: invalid dtensor element type

// -----

// Invalid: dtensor.dim axis out-of-bounds.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %d = "dtensor.dim"(%a) <{axis = 2 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
}

// VERIFY: dtensor.dim: axis 2 out of bounds for rank 2

// -----

// Invalid: dtensor.add dims are not SSA-identical.
builtin.module {
  %m0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %m1 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m0], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m1], f32>
  %x = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m0], f32>, !dtensor.tensor<[%m1], f32>) -> !dtensor.tensor<[%m0], f32>
}

// VERIFY: dtensor.add: expected pairwise SSA-identical dims for lhs/rhs

// -----

// Invalid: dtensor.matmul inner dim mismatch.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %k0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %k1 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %k0], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%k1, %n], f32>
  %x = "dtensor.matmul"(%a, %b)
    : (!dtensor.tensor<[%m, %k0], f32>, !dtensor.tensor<[%k1, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// VERIFY: dtensor.matmul: expected SSA-identical inner dims

// -----

// Invalid: strict cast forbids semantically-equal but non-identical dims.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %s0 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s1 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%s0], f32>
  %c = "dtensor.cast"(%a)
    : (!dtensor.tensor<[%s0], f32>) -> !dtensor.tensor<[%s1], f32>
}

// VERIFY: dtensor.cast: expected pairwise SSA-identical dims

// -----

// Invalid: dominance-in-types violation with non-dominating dim.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %u = "test.use"() : () -> !dtensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: ssa-dominance: value Value
// VERIFY: does not dominate its use in op `test.use`

// -----

// Parse: vector arity mismatch.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %v = "test.bad"() : () -> !dtensor.vector<%m, %n, f32>
}

// PARSE: Parse error at [[FILE]]

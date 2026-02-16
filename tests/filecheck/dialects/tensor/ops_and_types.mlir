// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid types and tensor ops.
builtin.module {
  %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 8 : i32}> : () -> !tensor.nat
  %k = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat

  %e = "tensor.empty"() : () -> !tensor.tensor<[%m, %n], f32>
  %c = "test.c"() : () -> f32
  %f = "tensor.fill"(%c) : (f32) -> !tensor.tensor<[%k, %k], f32>

  %a = "test.a"() : () -> !tensor.tensor<[%m, %n], f32>
  %b = "test.b"() : () -> !tensor.tensor<[%m, %n], f32>
  %sum = "tensor.add"(%a, %b)
    : (!tensor.tensor<[%m, %n], f32>, !tensor.tensor<[%m, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
  %prod = "tensor.mul"(%a, %b)
    : (!tensor.tensor<[%m, %n], f32>, !tensor.tensor<[%m, %n], f32>) -> !tensor.tensor<[%m, %n], f32>

  %lhs = "test.lhs"() : () -> !tensor.tensor<[%m, %k], f32>
  %rhs = "test.rhs"() : () -> !tensor.tensor<[%k, %n], f32>
  %mm = "tensor.matmul"(%lhs, %rhs)
    : (!tensor.tensor<[%m, %k], f32>, !tensor.tensor<[%k, %n], f32>) -> !tensor.tensor<[%m, %n], f32>

  %d0 = "tensor.dim"(%a) <{axis = 0 : i32}>
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.nat

  %cast = "tensor.cast"(%a)
    : (!tensor.tensor<[%m, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: "tensor.nat.const"
// CHECK: "tensor.empty"
// CHECK: "tensor.fill"
// CHECK: "tensor.add"
// CHECK: "tensor.mul"
// CHECK: "tensor.matmul"
// CHECK: "tensor.dim"
// CHECK: "tensor.cast"
// CHECK: }

// -----

// Invalid shape SSA sort.
builtin.module {
  %x = "arith.constant"() <{value = 1 : i32}> : () -> i32
  %t = "test.bad"() : () -> !tensor.tensor<[%x], f32>
}

// CHECK: shape SSA parameter must have type !tensor.nat, got i32

// -----

// Invalid element type.
builtin.module {
  %m = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %t = "test.bad_ten"() : () -> !tensor.tensor<[%m], tensor<1xf32>>
}

// CHECK: invalid tensor element type

// -----

// Invalid op invariant: add requires pairwise SSA-identical dims.
builtin.module {
  %m0 = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %m1 = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m0], f32>
  %b = "test.b"() : () -> !tensor.tensor<[%m1], f32>
  %c = "tensor.add"(%a, %b)
    : (!tensor.tensor<[%m0], f32>, !tensor.tensor<[%m1], f32>) -> !tensor.tensor<[%m0], f32>
}

// CHECK: tensor.add: expected pairwise SSA-identical dims for lhs/rhs

// -----

// Invalid matmul: inner dimensions must be SSA-identical.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %k0 = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %k1 = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 5 : i32}> : () -> !tensor.nat
  %a = "test.a"() : () -> !tensor.tensor<[%m, %k0], f32>
  %b = "test.b"() : () -> !tensor.tensor<[%k1, %n], f32>
  %c = "tensor.matmul"(%a, %b)
    : (!tensor.tensor<[%m, %k0], f32>, !tensor.tensor<[%k1, %n], f32>) -> !tensor.tensor<[%m, %n], f32>
}

// CHECK: tensor.matmul: expected SSA-identical inner dims

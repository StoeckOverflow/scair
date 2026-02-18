// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid types and dtensor ops.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %k = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat

  %e = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %c = "test.c"() : () -> f32
  %f = "dtensor.fill"(%c) : (f32) -> !dtensor.tensor<[%k, %k], f32>

  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m, %n], f32>
  %sum = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  %prod = "dtensor.mul"(%a, %b)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>

  %lhs = "test.lhs"() : () -> !dtensor.tensor<[%m, %k], f32>
  %rhs = "test.rhs"() : () -> !dtensor.tensor<[%k, %n], f32>
  %mm = "dtensor.matmul"(%lhs, %rhs)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>

  %d0 = "dtensor.dim"(%a) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.nat

  %cast = "dtensor.cast"(%a)
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK-LABEL: builtin.module {
// CHECK: "dtensor.nat.const"
// CHECK: "dtensor.empty"
// CHECK: "dtensor.fill"
// CHECK: "dtensor.add"
// CHECK: "dtensor.mul"
// CHECK: "dtensor.matmul"
// CHECK: "dtensor.dim"
// CHECK: "dtensor.cast"
// CHECK: }

// -----

// Invalid shape SSA sort.
builtin.module {
  %x = "arith.constant"() <{value = 1 : i32}> : () -> i32
  %t = "test.bad"() : () -> !dtensor.tensor<[%x], f32>
}

// CHECK: shape SSA parameter must have type !dtensor.nat, got i32

// -----

// Invalid element type.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %t = "test.bad_ten"() : () -> !dtensor.tensor<[%m], tensor<1xf32>>
}

// CHECK: invalid dtensor element type

// -----

// Invalid op invariant: add requires pairwise SSA-identical dims.
builtin.module {
  %m0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %m1 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m0], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%m1], f32>
  %c = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m0], f32>, !dtensor.tensor<[%m1], f32>) -> !dtensor.tensor<[%m0], f32>
}

// CHECK: dtensor.add: expected pairwise SSA-identical dims for lhs/rhs

// -----

// Invalid matmul: inner dimensions must be SSA-identical.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %k0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %k1 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 5 : i32}> : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %k0], f32>
  %b = "test.b"() : () -> !dtensor.tensor<[%k1, %n], f32>
  %c = "dtensor.matmul"(%a, %b)
    : (!dtensor.tensor<[%m, %k0], f32>, !dtensor.tensor<[%k1, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK: dtensor.matmul: expected SSA-identical inner dims

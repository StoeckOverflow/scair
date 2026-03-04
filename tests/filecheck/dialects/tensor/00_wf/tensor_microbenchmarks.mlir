// RUN: scair-opt %s | filecheck %s -DFILE=%s --check-prefix=VERIFY

func.func @ew_add(%m: !dtensor.nat, %n: !dtensor.nat) {
  %a = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %c = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  func.return %c : !dtensor.tensor<[%m, %n], f32>
}

func.func @saxpy(%n: !dtensor.nat, %a: f32) {
  %x = "dtensor.empty"() : () -> !dtensor.tensor<[%n], f32>
  %y = "dtensor.empty"() : () -> !dtensor.tensor<[%n], f32>
  %ax = "dtensor.fill"(%a) : (f32) -> !dtensor.tensor<[%n], f32>
  %prod = "dtensor.mul"(%ax, %x)
    : (!dtensor.tensor<[%n], f32>, !dtensor.tensor<[%n], f32>) -> !dtensor.tensor<[%n], f32>
  %sum = "dtensor.add"(%prod, %y)
    : (!dtensor.tensor<[%n], f32>, !dtensor.tensor<[%n], f32>) -> !dtensor.tensor<[%n], f32>
  func.return %sum : !dtensor.tensor<[%n], f32>
}

func.func @gemm(%m: !dtensor.nat, %k: !dtensor.nat, %n: !dtensor.nat) {
  %A = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %k], f32>
  %B = "dtensor.empty"() : () -> !dtensor.tensor<[%k, %n], f32>
  %C = "dtensor.matmul"(%A, %B)
    : (!dtensor.tensor<[%m, %k], f32>, !dtensor.tensor<[%k, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  func.return %C : !dtensor.tensor<[%m, %n], f32>
}

func.func @dim_identity(%m: !dtensor.nat, %n: !dtensor.nat) {
  %t = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %d0 = "dtensor.dim"(%t) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%m>
  %d1 = "dtensor.dim"(%t) <{axis = 1 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !value<%n>
  %u = "dtensor.empty"() : () -> !dtensor.tensor<[%d0, %d1], f32>
  %sum = "dtensor.nat.add"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  func.return %sum : !dtensor.nat
}

func.func @cast_id(%m: !dtensor.nat, %n: !dtensor.nat) {
  %x = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %y = "dtensor.cast"(%x) : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  func.return %y : !dtensor.tensor<[%m, %n], f32>
}

// VERIFY: builtin.module {
// VERIFY:   func.func @ew_add(%0: !dtensor.nat, %1: !dtensor.nat) {
// VERIFY:     %2 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:     %3 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:     %4 = "dtensor.add"(%2, %3) : (!dtensor.tensor<[%0, %1], f32>, !dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:     func.return %4 : !dtensor.tensor<[%0, %1], f32>
// VERIFY:   }
// VERIFY:   func.func @saxpy(%0: !dtensor.nat, %1: f32) {
// VERIFY:     %2 = "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// VERIFY:     %3 = "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// VERIFY:     %4 = "dtensor.fill"(%1) : (f32) -> !dtensor.tensor<[%0], f32>
// VERIFY:     %5 = "dtensor.mul"(%4, %2) : (!dtensor.tensor<[%0], f32>, !dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%0], f32>
// VERIFY:     %6 = "dtensor.add"(%5, %3) : (!dtensor.tensor<[%0], f32>, !dtensor.tensor<[%0], f32>) -> !dtensor.tensor<[%0], f32>
// VERIFY:     func.return %6 : !dtensor.tensor<[%0], f32>
// VERIFY:   }
// VERIFY:   func.func @gemm(%0: !dtensor.nat, %1: !dtensor.nat, %2: !dtensor.nat) {
// VERIFY:     %3 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:     %4 = "dtensor.empty"() : () -> !dtensor.tensor<[%1, %2], f32>
// VERIFY:     %5 = "dtensor.matmul"(%3, %4) : (!dtensor.tensor<[%0, %1], f32>, !dtensor.tensor<[%1, %2], f32>) -> !dtensor.tensor<[%0, %2], f32>
// VERIFY:     func.return %5 : !dtensor.tensor<[%0, %2], f32>
// VERIFY:   }
// VERIFY:   func.func @dim_identity(%0: !dtensor.nat, %1: !dtensor.nat) {
// VERIFY:     %2 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:     %3 = "dtensor.dim"(%2) <{axis = 0 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%0>
// VERIFY:     %4 = "dtensor.dim"(%2) <{axis = 1 : i32}> : (!dtensor.tensor<[%0, %1], f32>) -> !value<%1>
// VERIFY:     %5 = "dtensor.empty"() : () -> !dtensor.tensor<[%3, %4], f32>
// VERIFY:     %6 = "dtensor.nat.add"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:     func.return %6 : !dtensor.nat
// VERIFY:   }
// VERIFY:   func.func @cast_id(%0: !dtensor.nat, %1: !dtensor.nat) {
// VERIFY:     %2 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:     %3 = "dtensor.cast"(%2) : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:     func.return %3 : !dtensor.tensor<[%0, %1], f32>
// VERIFY:   }
// VERIFY: }

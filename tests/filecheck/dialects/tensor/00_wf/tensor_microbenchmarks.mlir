// RUN: scair-opt %s | filecheck %s -DFILE=%s --check-prefix=VERIFY

// elementwise add function
func.func @ew_add(%m: !dtensor.nat, %n: !dtensor.nat) {
  %a = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], f32>
  %c = "dtensor.add"(%a, %b)
    : (!dtensor.tensor<[%m, %n], f32>, !dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  func.return %c : !dtensor.tensor<[%m, %n], f32>
}

// SAXPY = a*x +y (scala-tensor + composition)
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

// "No-op cast" sanity (type equality via SSA identity)
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
// VERIFY:   func.func @cast_id(%0: !dtensor.nat, %1: !dtensor.nat) {
// VERIFY:     %2 = "dtensor.empty"() : () -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:     %3 = "dtensor.cast"(%2) : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%0, %1], f32>
// VERIFY:     func.return %3 : !dtensor.tensor<[%0, %1], f32>
// VERIFY:   }
// VERIFY: }

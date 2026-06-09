// RUN: scair-opt %s | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s | scair-opt --allow-unregistered-dialect --verify-diagnostics

// elementwise add function
func.func @ew_add(%m: !d_tensor.size, %n: !d_tensor.size) {
  %a = "d_tensor.empty"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %b = "d_tensor.empty"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %c = "d_tensor.add"(%a, %b)
    : (!d_tensor.tensor<[%m, %n], f32>, !d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  func.return %c : !d_tensor.tensor<[%m, %n], f32>
}

// SAXPY = a*x +y (scala-tensor + composition)
func.func @saxpy(%n: !d_tensor.size, %a: f32) {
  %x = "d_tensor.empty"() : () -> !d_tensor.tensor<[%n], f32>
  %y = "d_tensor.empty"() : () -> !d_tensor.tensor<[%n], f32>
  %ax = "d_tensor.fill"(%a) : (f32) -> !d_tensor.tensor<[%n], f32>
  %prod = "d_tensor.mul"(%ax, %x)
    : (!d_tensor.tensor<[%n], f32>, !d_tensor.tensor<[%n], f32>) -> !d_tensor.tensor<[%n], f32>
  %sum = "d_tensor.add"(%prod, %y)
    : (!d_tensor.tensor<[%n], f32>, !d_tensor.tensor<[%n], f32>) -> !d_tensor.tensor<[%n], f32>
  func.return %sum : !d_tensor.tensor<[%n], f32>
}

// "No-op cast" sanity (type equality via SSA identity)
func.func @cast_id(%m: !d_tensor.size, %n: !d_tensor.size) {
  %x = "d_tensor.empty"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %y = "d_tensor.cast"(%x) : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  func.return %y : !d_tensor.tensor<[%m, %n], f32>
}

// VERIFY: builtin.module {
// VERIFY:   func.func @ew_add(%0: !d_tensor.size, %1: !d_tensor.size) {
// VERIFY:     %2 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:     %3 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:     %4 = "d_tensor.add"(%2, %3) : (!d_tensor.tensor<[%0, %1], f32>, !d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:     func.return %4 : !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   }
// VERIFY:   func.func @saxpy(%0: !d_tensor.size, %1: f32) {
// VERIFY:     %2 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0], f32>
// VERIFY:     %3 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0], f32>
// VERIFY:     %4 = "d_tensor.fill"(%1) : (f32) -> !d_tensor.tensor<[%0], f32>
// VERIFY:     %5 = "d_tensor.mul"(%4, %2) : (!d_tensor.tensor<[%0], f32>, !d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>
// VERIFY:     %6 = "d_tensor.add"(%5, %3) : (!d_tensor.tensor<[%0], f32>, !d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>
// VERIFY:     func.return %6 : !d_tensor.tensor<[%0], f32>
// VERIFY:   }
// VERIFY:   func.func @cast_id(%0: !d_tensor.size, %1: !d_tensor.size) {
// VERIFY:     %2 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:     %3 = "d_tensor.cast"(%2) : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:     func.return %3 : !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   }
// VERIFY: }

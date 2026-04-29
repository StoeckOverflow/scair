// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dtensor-to-dmemref-shape-preserving | filecheck %s -DFILE=%s

// Multiple split groups and a 3-factor group lower to one reinterpret_cast with
// generic row-major expanded strides.
builtin.module {
  %a = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %c = "dtensor.nat.param"() : () -> !dtensor.nat
  %d = "dtensor.nat.param"() : () -> !dtensor.nat
  %e = "dtensor.nat.param"() : () -> !dtensor.nat
  %ab = "dtensor.nat.mul"(%a, %b) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %de = "dtensor.nat.mul"(%d, %e) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %cde = "dtensor.nat.mul"(%c, %de) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%ab, %cde], f32>
  %q5 = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32, 4 : i32]]}>
    : (!dtensor.tensor<[%ab, %cde], f32>) -> !dtensor.tensor<[%a, %b, %c, %d, %e], f32>
  "test.keep"(%q5) : (!dtensor.tensor<[%a, %b, %c, %d, %e], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %4 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %5 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %6 = "dtensor.nat.mul"(%3, %4) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %7 = "dtensor.nat.mul"(%2, %6) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %8 = "test.q"() : () -> !dtensor.tensor<[%5, %7], f32>
// CHECK-NEXT:   %9 = "builtin.unrealized_conversion_cast"(%8) : (!dtensor.tensor<[%5, %7], f32>) -> !d_memref.memref<[%5, %7], f32>
// CHECK-NEXT:   %10 = "dtensor.shape.to_index"(%5) : (!dtensor.nat) -> index
// CHECK-NEXT:   %11 = "dtensor.shape.to_index"(%7) : (!dtensor.nat) -> index
// CHECK-NEXT:   %12 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK-NEXT:   %13 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK-NEXT:   %14 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
// CHECK-NEXT:   %15 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// CHECK-NEXT:   %16 = "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
// CHECK-NEXT:   %17 = "arith.muli"(%11, %13) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:   %18 = "arith.muli"(%16, %15) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:   %19 = d_memref.reinterpret_cast %9
// CHECK-NEXT:   : !d_memref.memref<[%5, %7], f32> to !d_memref.memref<[%0, %1, %2, %3, %4], f32, offset: 0 : index, strides: [%17, %11, %18, %16, 1 : index]>
// CHECK-NEXT:   %20 = "builtin.unrealized_conversion_cast"(%19) : (!d_memref.memref<[%0, %1, %2, %3, %4], f32, offset: 0 : index, strides: [%17, %11, %18, %16, 1 : index]>) -> !dtensor.tensor<[%0, %1, %2, %3, %4], f32>
// CHECK-NEXT:   "test.keep"(%20) : (!dtensor.tensor<[%0, %1, %2, %3, %4], f32>) -> ()
// CHECK-NEXT: }

// -----

// Rank-preserving expand_shape lowers to equivalent row-major reinterpret metadata.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%m, %n], f32>
  %same = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32]]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%same) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "test.q"() : () -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %3 = "builtin.unrealized_conversion_cast"(%2) : (!dtensor.tensor<[%0, %1], f32>) -> !d_memref.memref<[%0, %1], f32>
// CHECK-NEXT:   %4 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK-NEXT:   %5 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK-NEXT:   %6 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK-NEXT:   %7 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK-NEXT:   %8 = d_memref.reinterpret_cast %3
// CHECK-NEXT:   : !d_memref.memref<[%0, %1], f32> to !d_memref.memref<[%0, %1], f32, offset: 0 : index, strides: [%5, 1 : index]>
// CHECK-NEXT:   %9 = "builtin.unrealized_conversion_cast"(%8) : (!d_memref.memref<[%0, %1], f32, offset: 0 : index, strides: [%5, 1 : index]>) -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   "test.keep"(%9) : (!dtensor.tensor<[%0, %1], f32>) -> ()
// CHECK-NEXT: }

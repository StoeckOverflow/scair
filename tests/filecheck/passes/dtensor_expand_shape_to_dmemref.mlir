// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-to-dmemref-shape-preserving | filecheck %s -DFILE=%s

builtin.module {
  %b = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.param"() : () -> !dtensor.nat
  %heads = "dtensor.nat.param"() : () -> !dtensor.nat
  %head_dim = "dtensor.nat.param"() : () -> !dtensor.nat
  %hidden = "dtensor.nat.mul"(%heads, %head_dim) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %q = "test.q"() : () -> !dtensor.tensor<[%b, %s, %hidden], f32>
  %q4 = "dtensor.expand_shape"(%q)
    <{reassociation = [[0 : i32], [1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%b, %s, %hidden], f32>) -> !dtensor.tensor<[%b, %s, %heads, %head_dim], f32>
  "test.keep"(%q4) : (!dtensor.tensor<[%b, %s, %heads, %head_dim], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %4 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %5 = "test.q"() : () -> !dtensor.tensor<[%0, %1, %4], f32>
// CHECK-NEXT:   %6 = "builtin.unrealized_conversion_cast"(%5) : (!dtensor.tensor<[%0, %1, %4], f32>) -> !d_memref.memref<[%0, %1, %4], f32>
// CHECK-NEXT:   %7 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK-NEXT:   %8 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK-NEXT:   %9 = "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
// CHECK-NEXT:   %10 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK-NEXT:   %11 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK-NEXT:   %12 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
// CHECK-NEXT:   %13 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// CHECK-NEXT:   %14 = "arith.muli"(%9, %8) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:   %15 = d_memref.reinterpret_cast %6
// CHECK-NEXT:   : !d_memref.memref<[%0, %1, %4], f32> to !d_memref.memref<[%0, %1, %2, %3], f32, offset: 0 : index, strides: [%14, %9, %13, 1 : index]>
// CHECK-NEXT:   %16 = "builtin.unrealized_conversion_cast"(%15) : (!d_memref.memref<[%0, %1, %2, %3], f32, offset: 0 : index, strides: [%14, %9, %13, 1 : index]>) -> !dtensor.tensor<[%0, %1, %2, %3], f32>
// CHECK-NEXT:   "test.keep"(%16) : (!dtensor.tensor<[%0, %1, %2, %3], f32>) -> ()
// CHECK-NEXT: }

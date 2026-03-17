// RUN: scair-opt %s --allow-unregistered-dialect -p refine-memref-layout-types,canonicalize-dependent-layouts | filecheck %s

builtin.module {
  func.func @pipeline(%stride0 : index, %stride1 : index) {
    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
    %d0_i = "dtensor.shape.to_index"(%d0) : (!dtensor.nat) -> index
    %d1_i = "dtensor.shape.to_index"(%d1) : (!dtensor.nat) -> index
    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %src = d_memref.alloc : () -> !d_memref.memref<[%d0, %d1], f32>
    %sv = d_memref.subview %src[%zero, %zero][%d0_i, %d1_i][%stride0, %stride1] : !d_memref.memref<[%d0, %d1], f32> -> !d_memref.memref<[%d0, %d1], f32>
    %v = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
    d_memref.store %v, %sv[%zero, %zero] : f32, !d_memref.memref<[%d0, %d1], f32>
    %r = d_memref.load %sv[%zero, %zero] : !d_memref.memref<[%d0, %d1], f32> -> f32
    "test.keep"(%r) : (f32) -> ()
    func.return
  }
}

// CHECK-LABEL: func.func @pipeline(%0: index, %1: index) {
// CHECK-NEXT:    %2 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %3 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %4 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
// CHECK-NEXT:    %5 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// CHECK-NEXT:    %6 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %7 = d_memref.alloc : () -> !d_memref.memref<[%2, %3], f32>
// CHECK-NEXT:    %8 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// CHECK-NEXT:    %9 = "arith.muli"(%0, %8) : (index, index) -> index
// CHECK-NEXT:    %10 = d_memref.subview %7[%6, %6][%4, %5][%0, %1] : !d_memref.memref<[%2, %3], f32> -> !d_memref.memref<[%2, %3], f32, offset: 0 : index, strides: [%9, %1]>
// CHECK-NEXT:    %11 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
// CHECK-NEXT:    d_memref.store %11, %10[%6, %6] : f32, !d_memref.memref<[%2, %3], f32, offset: 0 : index, strides: [%9, %1]>
// CHECK-NEXT:    %12 = d_memref.load %10[%6, %6] : !d_memref.memref<[%2, %3], f32, offset: 0 : index, strides: [%9, %1]> -> f32
// CHECK-NEXT:    "test.keep"(%12) : (f32) -> ()
// CHECK-NEXT:    func.return
// CHECK-NEXT:  }

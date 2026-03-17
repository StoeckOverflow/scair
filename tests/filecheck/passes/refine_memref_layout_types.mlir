// RUN: scair-opt %s -p refine-memref-layout-types | filecheck %s

builtin.module {
  func.func @refine(%stride0 : index, %stride1 : index, %off0 : index, %off1 : index) {
    %d0 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
    %d0_i = "dtensor.shape.to_index"(%d0) : (!dtensor.nat) -> index
    %d1_i = "dtensor.shape.to_index"(%d1) : (!dtensor.nat) -> index
    %src = d_memref.alloc : () -> !d_memref.memref<[%d0, %d1], f32>
    %sv = d_memref.subview %src[%off0, %off1][%d0_i, %d1_i][%stride0, %stride1] : !d_memref.memref<[%d0, %d1], f32> -> !d_memref.memref<[%d0, %d1], f32>
    func.return
  }
}

// CHECK-LABEL: func.func @refine(%0: index, %1: index, %2: index, %3: index) {
// CHECK-NEXT:    %4 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %5 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %6 = "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
// CHECK-NEXT:    %7 = "dtensor.shape.to_index"(%5) : (!dtensor.nat) -> index
// CHECK-NEXT:    %8 = d_memref.alloc : () -> !d_memref.memref<[%4, %5], f32>
// CHECK-NEXT:    %9 = "dtensor.shape.to_index"(%5) : (!dtensor.nat) -> index
// CHECK-NEXT:    %10 = "arith.muli"(%2, %9) : (index, index) -> index
// CHECK-NEXT:    %11 = "arith.addi"(%10, %3) : (index, index) -> index
// CHECK-NEXT:    %12 = "dtensor.shape.to_index"(%5) : (!dtensor.nat) -> index
// CHECK-NEXT:    %13 = "arith.muli"(%0, %12) : (index, index) -> index
// CHECK-NEXT:    %14 = d_memref.subview %8[%2, %3][%6, %7][%0, %1] : !d_memref.memref<[%4, %5], f32> -> !d_memref.memref<[%4, %5], f32, offset: %11, strides: [%13, %1]>
// CHECK-NEXT:    func.return
// CHECK-NEXT:  }

builtin.module {
  func.func @refine_reinterpret(%stride0 : index, %stride1 : index) {
    %d0 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
    %d0_i = "dtensor.shape.to_index"(%d0) : (!dtensor.nat) -> index
    %d1_i = "dtensor.shape.to_index"(%d1) : (!dtensor.nat) -> index
    %flat = "dtensor.nat.const"() <{value = 128 : i32}> : () -> !dtensor.nat
    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %src = d_memref.alloc : () -> !d_memref.memref<[%flat], f32>
    %rc = d_memref.reinterpret_cast %src to
      offset: [%zero],
      sizes: [%d0_i, %d1_i],
      strides: [%stride0, %stride1]
    : !d_memref.memref<[%flat], f32> to !d_memref.memref<[%d0, %d1], f32>
    func.return
  }
}

// CHECK-LABEL: func.func @refine_reinterpret(%0: index, %1: index) {
// CHECK-NEXT:    %2 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %3 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %4 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
// CHECK-NEXT:    %5 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// CHECK-NEXT:    %6 = "dtensor.nat.const"() <{value = 128 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %7 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %8 = d_memref.alloc : () -> !d_memref.memref<[%6], f32>
// CHECK-NEXT:    %9 = d_memref.reinterpret_cast %8 to
// CHECK-NEXT:      offset: [%7],
// CHECK-NEXT:      sizes: [%4, %5],
// CHECK-NEXT:      strides: [%0, %1]
// CHECK-NEXT:    : !d_memref.memref<[%6], f32> to !d_memref.memref<[%2, %3], f32, offset: %7, strides: [%0, %1]>
// CHECK-NEXT:    func.return
// CHECK-NEXT:  }

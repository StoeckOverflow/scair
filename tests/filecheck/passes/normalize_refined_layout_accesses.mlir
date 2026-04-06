// RUN: scair-opt %s -p normalize-refined-layout-accesses | filecheck %s

builtin.module {
  func.func @normalize(%stride0 : index, %stride1 : index, %i : index, %j : index) -> f32 {
    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
    %flat = "dtensor.nat.const"() <{value = 262144 : i32}> : () -> !dtensor.nat
    %buf = d_memref.alloc : () -> !d_memref.memref<[%flat], f32>
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %view = d_memref.reinterpret_cast %buf
    : !d_memref.memref<[%flat], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]>
    %v = d_memref.load %view[%i, %j] : !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]> -> f32
    func.return %v : f32
  }
}

// CHECK-LABEL: func.func @normalize(%0: index, %1: index, %2: index, %3: index) -> f32 {
// CHECK-NEXT:    %4 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %5 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %6 = "dtensor.nat.const"() <{value = 262144 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %7 = d_memref.alloc : () -> !d_memref.memref<[%6], f32>
// CHECK-NEXT:    %8 = "arith.constant"() <{value = 256 : index}> : () -> index
// CHECK-NEXT:    %9 = "arith.constant"() <{value = 1024 : index}> : () -> index
// CHECK-NEXT:    %10 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %11 = d_memref.reinterpret_cast %7
// CHECK-NEXT:    : !d_memref.memref<[%6], f32> to !d_memref.memref<[%4, %5], f32, offset: %10, strides: [%0, %1]>
// CHECK-NEXT:    %12 = "arith.muli"(%2, %0) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %13 = "arith.muli"(%3, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %14 = "arith.addi"(%10, %12) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %15 = "arith.addi"(%14, %13) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %16 = d_memref.load %7[%15] : !d_memref.memref<[%6], f32> -> f32
// CHECK-NEXT:    func.return %16 : f32
// CHECK-NEXT:  }

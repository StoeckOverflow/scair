// RUN: scair-opt %s -p expand-refined-strided-metadata | filecheck %s

builtin.module {
  func.func @expand(%stride0 : index, %stride1 : index) -> f32 {
    %c256 = "llvm.mlir.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "llvm.mlir.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
    %total = "llvm.mul"(%c256, %stride0) : (index, index) -> index
    %flat_nat = "dtensor.index_to_nat"(%total) : (index) -> !dtensor.nat
    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
    %d0_i = "dtensor.shape.to_index"(%d0) : (!dtensor.nat) -> index
    %d1_i = "dtensor.shape.to_index"(%d1) : (!dtensor.nat) -> index
    %flat = d_memref.alloc : () -> !d_memref.memref<[%flat_nat], f32>
    %buf = d_memref.reinterpret_cast %flat to
      offset: [%c0],
      sizes: [%d0_i, %d1_i],
      strides: [%stride0, %stride1]
    : !d_memref.memref<[%flat_nat], f32> to !d_memref.memref<[%d0, %d1], f32, offset: 0, strides: [%stride0, %stride1]>
    %v = d_memref.load %buf[%c0, %c0] : !d_memref.memref<[%d0, %d1], f32, offset: 0, strides: [%stride0, %stride1]> -> f32
    d_memref.dealloc %flat : !d_memref.memref<[%flat_nat], f32>
    func.return %v : f32
  }
}

// CHECK-LABEL: func.func @expand(%0: index, %1: index) -> f32 {
// CHECK-NEXT:    %2 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %3 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %4 = llvm.mlir.constant 0 : index : index
// CHECK-NEXT:    %5 = "llvm.mul"(%2, %0) : (index, index) -> index
// CHECK-NEXT:    %6 = "dtensor.index_to_nat"(%5) : (index) -> !dtensor.nat
// CHECK-NEXT:    %7 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %8 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %9 = "dtensor.shape.to_index"(%7) : (!dtensor.nat) -> index
// CHECK-NEXT:    %10 = "dtensor.shape.to_index"(%8) : (!dtensor.nat) -> index
// CHECK-NEXT:    %11 = d_memref.alloc : () -> !d_memref.memref<[%6], f32>
// CHECK-NEXT:    %12, %13, %14, %15 = "d_memref.extract_strided_metadata"(%11) : (!d_memref.memref<[%6], f32>) -> (!d_memref.memref<[], f32>, index, index, index)
// CHECK-NEXT:    %16 = d_memref.reinterpret_cast %12 to
// CHECK-NEXT:      offset: [%4],
// CHECK-NEXT:      sizes: [%9, %10],
// CHECK-NEXT:      strides: [%0, %1]
// CHECK-NEXT:    : !d_memref.memref<[], f32> to !d_memref.memref<[%7, %8], f32, offset: 0, strides: [%0, %1]>
// CHECK-NEXT:    %17 = d_memref.load %16[%4, %4] : !d_memref.memref<[%7, %8], f32, offset: 0, strides: [%0, %1]> -> f32
// CHECK-NEXT:    d_memref.dealloc %11 : !d_memref.memref<[%6], f32>
// CHECK-NEXT:    func.return %17 : f32
// CHECK-NEXT:  }

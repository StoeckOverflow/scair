// RUN: scair-opt %s -p finalize-refined-dmemref-to-llvm | filecheck %s

builtin.module {
  func.func @finalize(%stride0 : index, %stride1 : index) -> f32 {
    %c1024 = "llvm.mlir.constant"() <{value = 1024 : index}> : () -> index
    %c1 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
    %c0 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
    %c256 = "llvm.mlir.constant"() <{value = 256 : index}> : () -> index
    %total = "llvm.mul"(%c256, %stride0) : (index, index) -> index
    %flat_nat = "dtensor.index_to_nat"(%total) : (index) -> !dtensor.nat
    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
    %d0_i = "dtensor.shape.to_index"(%d0) : (!dtensor.nat) -> index
    %d1_i = "dtensor.shape.to_index"(%d1) : (!dtensor.nat) -> index
    %flat = d_memref.alloc : () -> !d_memref.memref<[%flat_nat], f32>
    %base, %off, %size0, %stride = "d_memref.extract_strided_metadata"(%flat) : (!d_memref.memref<[%flat_nat], f32>) -> (!d_memref.memref<[], f32>, index, index, index)
    %buf = d_memref.reinterpret_cast %base
    : !d_memref.memref<[], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]>
    %v = d_memref.load %buf[%c0, %c0] : !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]> -> f32
    d_memref.dealloc %flat : !d_memref.memref<[%flat_nat], f32>
    func.return %v : f32
  }
}

// CHECK-LABEL: func.func @finalize(%0: index, %1: index) -> f32 {
// CHECK-NEXT:    %2 = llvm.mlir.constant 0 : index : index
// CHECK-NEXT:    %3 = "llvm.add"(%0, %2) : (index, index) -> index
// CHECK-NEXT:    %4 = "llvm.add"(%1, %2) : (index, index) -> index
// CHECK-NEXT:    %5 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %6 = llvm.mlir.constant 1 : index : index
// CHECK-NEXT:    %7 = llvm.mlir.constant 0 : index : index
// CHECK-NEXT:    %8 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %9 = "llvm.mul"(%8, %3) : (index, index) -> index
// CHECK-NEXT:    %10 = llvm.mlir.zero : !llvm.ptr
// CHECK-NEXT:    %11 = "llvm.getelementptr"(%10, %9) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %12 = "llvm.ptrtoint"(%11) : (!llvm.ptr) -> index
// CHECK-NEXT:    %13 = "llvm.call"(%12) <{callee = @malloc}> : (index) -> !llvm.ptr
// CHECK-NEXT:    %14 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %15 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %16 = "llvm.mul"(%7, %3) : (index, index) -> index
// CHECK-NEXT:    %17 = "llvm.mul"(%7, %4) : (index, index) -> index
// CHECK-NEXT:    %18 = "llvm.add"(%16, %17) : (index, index) -> index
// CHECK-NEXT:    %19 = "llvm.getelementptr"(%13, %18) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %20 = llvm.load %19 : !llvm.ptr -> f32
// CHECK-NEXT:    "llvm.call"(%13) <{callee = @free}> : (!llvm.ptr) -> ()
// CHECK-NEXT:    "llvm.return"(%20) : (f32) -> ()
// CHECK-NEXT:  }

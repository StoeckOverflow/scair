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

// CHECK-LABEL: func.func @finalize(%0: i64, %1: i64) -> f32 {
// CHECK: %2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK: %9 = "llvm.mul"(%8, %3) : (i64, i64) -> i64
// CHECK: %11 = "llvm.getelementptr"(%10, %9) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %12 = "llvm.ptrtoint"(%11) : (!llvm.ptr) -> i64
// CHECK: llvm.call @malloc(%12) : (i64) -> !llvm.ptr
// CHECK: llvm.call @free(%13) : (!llvm.ptr) -> ()
// CHECK: "llvm.return"(%20) : (f32) -> ()

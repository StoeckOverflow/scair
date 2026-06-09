// RUN: scair-opt %s -p finalize-refined-d-memref-to-llvm | filecheck %s

builtin.module {
  func.func @finalize(%stride0 : index, %stride1 : index) -> f32 {
    %c1024 = "llvm.mlir.constant"() <{value = 1024 : index}> : () -> index
    %c1 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
    %c0 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
    %c256 = "llvm.mlir.constant"() <{value = 256 : index}> : () -> index
    %total = "llvm.mul"(%c256, %stride0) : (index, index) -> index
    %flat_size = "d_tensor.size.import"(%total) : (index) -> !d_tensor.size
    %d0 = "d_tensor.size.constant"() <{value = 256 : i32}> : () -> !d_tensor.size
    %d1 = "d_tensor.size.constant"() <{value = 1024 : i32}> : () -> !d_tensor.size
    %flat = d_memref.alloc : () -> !d_memref.memref<[%flat_size], f32>
    %base, %off, %size0, %stride = "d_memref.extract_strided_metadata"(%flat) : (!d_memref.memref<[%flat_size], f32>) -> (!d_memref.memref<[], f32>, index, index, index)
    %buf = d_memref.reinterpret_cast %base
    : !d_memref.memref<[], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]>
    %v = d_memref.load %buf[%c0, %c0] : !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]> -> f32
    d_memref.dealloc %flat : !d_memref.memref<[%flat_size], f32>
    func.return %v : f32
  }
}

// CHECK-LABEL: func.func @finalize(%0: i64, %1: i64) -> f32 {
// CHECK: %2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK: %9 = "llvm.mul"(%8, %3) : (i64, i64) -> i64
// CHECK: %13 = "llvm.getelementptr"(%12, %9) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %14 = "llvm.ptrtoint"(%13) : (!llvm.ptr) -> i64
// CHECK: llvm.call @malloc(%14) : (i64) -> !llvm.ptr
// CHECK: llvm.call @free(%15) : (!llvm.ptr) -> ()
// CHECK: "llvm.return"(%20) : (f32) -> ()

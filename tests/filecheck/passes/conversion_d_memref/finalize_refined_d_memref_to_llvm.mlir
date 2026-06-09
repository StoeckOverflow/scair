// RUN: scair-opt %s -p finalize-refined-d-memref-to-llvm | filecheck %s

builtin.module {
  func.func @finalize(%stride0 : index, %stride1 : index) -> f32 {
    %c1024 = "llvm.mlir.constant"() <{value = 1024 : index}> : () -> index
    %c1 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
    %c0 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
    %c256 = "llvm.mlir.constant"() <{value = 256 : index}> : () -> index
    %total = "llvm.mul"(%c256, %stride0) : (index, index) -> index
    %flat = d_memref.alloc : () -> !d_memref.memref<[%total], f32>
    %base, %off, %size0, %stride = "d_memref.extract_strided_metadata"(%flat) : (!d_memref.memref<[%total], f32>) -> (!d_memref.memref<[], f32>, index, index, index)
    %buf = d_memref.reinterpret_cast %base
    : !d_memref.memref<[], f32> to !d_memref.memref<[%c256, %c1024], f32, offset: %c0, strides: [%stride0, %stride1]>
    %v = d_memref.load %buf[%c0, %c0] : !d_memref.memref<[%c256, %c1024], f32, offset: %c0, strides: [%stride0, %stride1]> -> f32
    d_memref.dealloc %flat : !d_memref.memref<[%total], f32>
    func.return %v : f32
  }
}

// CHECK-LABEL: func.func @finalize(%0: i64, %1: i64) -> f32 {
// CHECK: %[[ZERO:[0-9]+]] = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK: %[[STRIDE0:[0-9]+]] = "llvm.add"(%0, %[[ZERO]]) : (i64, i64) -> i64
// CHECK: %[[D0:[0-9]+]] = "llvm.mlir.constant"() <{value = 256}> : () -> i64
// CHECK: %[[TOTAL:[0-9]+]] = "llvm.mul"(%[[D0]], %[[STRIDE0]]) : (i64, i64) -> i64
// CHECK: %[[BASE:[0-9]+]] = "llvm.mlir.zero"() : () -> !llvm.ptr
// CHECK: %[[BYTES_PTR:[0-9]+]] = "llvm.getelementptr"(%[[BASE]], %[[TOTAL]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[BYTES:[0-9]+]] = "llvm.ptrtoint"(%[[BYTES_PTR]]) : (!llvm.ptr) -> i64
// CHECK: %[[PTR:[0-9]+]] = llvm.call @malloc(%[[BYTES]]) : (i64) -> !llvm.ptr
// CHECK: %[[LOAD_PTR:[0-9]+]] = "llvm.getelementptr"(%[[PTR]], %{{[0-9]+}}) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[VAL:[0-9]+]] = llvm.load %[[LOAD_PTR]] : !llvm.ptr -> f32
// CHECK: llvm.call @free(%[[PTR]]) : (!llvm.ptr) -> ()
// CHECK: "llvm.return"(%[[VAL]]) : (f32) -> ()

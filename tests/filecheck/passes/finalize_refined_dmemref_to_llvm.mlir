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
// CHECK-NEXT:    %2 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %3 = llvm.mlir.constant 1 : index : index
// CHECK-NEXT:    %4 = llvm.mlir.constant 0 : index : index
// CHECK-NEXT:    %5 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %6 = "llvm.mul"(%5, %0) : (index, index) -> index
// CHECK-NEXT:    %7 = llvm.mlir.zero : !llvm.ptr
// CHECK-NEXT:    %8 = "llvm.getelementptr"(%7, %6) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %9 = "llvm.ptrtoint"(%8) : (!llvm.ptr) -> index
// CHECK-NEXT:    %10 = "llvm.call"(%9) <{callee = @malloc}> : (index) -> !llvm.ptr
// CHECK-NEXT:    %11 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// CHECK-NEXT:    %12 = "llvm.insertvalue"(%10, %11) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// CHECK-NEXT:    %13 = "llvm.insertvalue"(%10, %12) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// CHECK-NEXT:    %14 = "llvm.insertvalue"(%4, %13) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// CHECK-NEXT:    %15 = "llvm.insertvalue"(%6, %14) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// CHECK-NEXT:    %16 = "llvm.insertvalue"(%3, %15) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// CHECK-NEXT:    %17 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %18 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %19 = "llvm.extractvalue"(%16) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// CHECK-NEXT:    %20 = "llvm.extractvalue"(%16) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// CHECK-NEXT:    %21 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %22 = "llvm.insertvalue"(%19, %21) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %23 = "llvm.insertvalue"(%20, %22) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %24 = "llvm.insertvalue"(%4, %23) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %25 = "llvm.insertvalue"(%17, %24) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %26 = "llvm.insertvalue"(%18, %25) <{position = array<i32: 3, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %27 = "llvm.insertvalue"(%0, %26) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %28 = "llvm.insertvalue"(%1, %27) <{position = array<i32: 4, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %29 = "llvm.extractvalue"(%28) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// CHECK-NEXT:    %30 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// CHECK-NEXT:    %31 = "llvm.mul"(%4, %30) : (index, index) -> index
// CHECK-NEXT:    %32 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// CHECK-NEXT:    %33 = "llvm.mul"(%4, %32) : (index, index) -> index
// CHECK-NEXT:    %34 = "llvm.add"(%31, %33) : (index, index) -> index
// CHECK-NEXT:    %35 = "llvm.getelementptr"(%29, %34) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %36 = llvm.load %35 : !llvm.ptr -> f32
// CHECK-NEXT:    %37 = "llvm.extractvalue"(%16) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// CHECK-NEXT:    "llvm.call"(%37) <{callee = @free}> : (!llvm.ptr) -> ()
// CHECK-NEXT:    "llvm.return"(%36) : (f32) -> ()
// CHECK-NEXT:  }

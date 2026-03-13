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
    %flat = "llvm.refined.alloc_descriptor"() <{source_type = !d_memref.memref<[%flat_nat], f32>}> : () -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
    %buf = "llvm.refined.reinterpret_descriptor"(%flat, %c0, %c256, %c1024, %stride0, %stride1) <{source_type = !d_memref.memref<[%flat_nat], f32>, target_type = !d_memref.memref<[%d0, %d1], f32, offset: 0, strides: [%stride0, %stride1]>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>, index, index, index, index, index) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
    %v = "llvm.refined.load"(%buf, %c0, %c0) <{source_type = !d_memref.memref<[%d0, %d1], f32, offset: 0, strides: [%stride0, %stride1]>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>, index, index) -> f32
    "llvm.refined.dealloc"(%flat) : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> ()
    func.return %v : f32
  }
}

// CHECK-LABEL: func.func @finalize
// CHECK: %[[NULL:.*]] = llvm.mlir.zero : !llvm.ptr
// CHECK: %[[SIZEPTR:.*]] = "llvm.getelementptr"(%[[NULL]], %6) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK: %[[BYTES:.*]] = "llvm.ptrtoint"(%[[SIZEPTR]]) : (!llvm.ptr) -> index
// CHECK: %[[MALLOC:.*]] = "llvm.call"(%[[BYTES]]) <{callee = @malloc}> : (index) -> !llvm.ptr
// CHECK: %[[DESC1:.*]] = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// CHECK: %[[R1:.*]] = "llvm.insertvalue"(%3, %{{.*}}) <{position = array<i32: 4, 0>}>
// CHECK: %[[DESC2:.*]] = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK: %[[IDX0:.*]] = "llvm.mul"(%4, %{{.*}}) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK: %[[IDX1:.*]] = "llvm.mul"(%4, %{{.*}}) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK: %[[LIN:.*]] = "llvm.add"(%[[IDX0]], %[[IDX1]]) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK: %[[GEP:.*]] = "llvm.getelementptr"(%{{.*}}, %[[LIN]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK: %[[PTR:.*]] = "llvm.extractvalue"(%[[R1]]) <{position = array<i32: 0>}>
// CHECK: "llvm.call"(%[[PTR]]) <{callee = @free}> : (!llvm.ptr) -> ()
// CHECK: "llvm.return"(%{{.*}}) : (f32) -> ()

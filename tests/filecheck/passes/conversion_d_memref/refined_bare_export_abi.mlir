// RUN: scair-opt %s --passes lower-d-memref-to-llvm,convert-func-to-llvm,convert-llvm-export-abi | filecheck %s
// RUN: scair-opt %s --passes lower-d-memref-to-llvm,convert-func-to-llvm,convert-llvm-export-abi | scair-opt --verify-diagnostics

builtin.module {
  func.func @refined_rank1(
    %n_size : !d_tensor.size,
    %buf : !d_memref.memref<[%n_size], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %x = d_memref.load %buf[%c0] : !d_memref.memref<[%n_size], f32> -> f32
    d_memref.store %x, %buf[%c0] : f32, !d_memref.memref<[%n_size], f32>
    func.return
  }

  func.func @refined_rank2(
    %m_size : !d_tensor.size,
    %n_size : !d_tensor.size,
    %buf : !d_memref.memref<[%m_size, %n_size], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %x = d_memref.load %buf[%c0, %c0] : !d_memref.memref<[%m_size, %n_size], f32> -> f32
    d_memref.store %x, %buf[%c0, %c0] : f32, !d_memref.memref<[%m_size, %n_size], f32>
    func.return
  }
}

// CHECK-LABEL: llvm.func @refined_rank1(
// CHECK-SAME: %[[N:[0-9]+]]: i64, %[[PTR:[0-9]+]]: !llvm.ptr
// CHECK: %[[ZERO:[0-9]+]] = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK: %[[LOAD_PTR:[0-9]+]] = "llvm.getelementptr"(%[[PTR]], %[[ZERO]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[VAL:[0-9]+]] = llvm.load %[[LOAD_PTR]] : !llvm.ptr -> f32
// CHECK: %[[STORE_PTR:[0-9]+]] = "llvm.getelementptr"(%[[PTR]], %[[ZERO]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: "llvm.store"(%[[VAL]], %[[STORE_PTR]]) : (f32, !llvm.ptr) -> ()
// CHECK: "llvm.return"() : () -> ()

// CHECK-LABEL: llvm.func @refined_rank2(
// CHECK-SAME: %[[M:[0-9]+]]: i64, %[[N2:[0-9]+]]: i64, %[[PTR2:[0-9]+]]: !llvm.ptr
// CHECK: %[[ZERO2:[0-9]+]] = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK: %[[ONE:[0-9]+]] = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// CHECK: %[[STRIDE0:[0-9]+]] = "llvm.mul"(%[[N2]], %[[ONE]]) : (i64, i64) -> i64
// CHECK: %[[ROW_OFF:[0-9]+]] = "llvm.mul"(%[[ZERO2]], %[[STRIDE0]]) : (i64, i64) -> i64
// CHECK: %[[LINEAR0:[0-9]+]] = "llvm.add"(%[[ROW_OFF]], %[[ZERO2]]) : (i64, i64) -> i64
// CHECK: %[[LOAD_PTR2:[0-9]+]] = "llvm.getelementptr"(%[[PTR2]], %[[LINEAR0]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[VAL2:[0-9]+]] = llvm.load %[[LOAD_PTR2]] : !llvm.ptr -> f32
// CHECK: %[[STRIDE0_B:[0-9]+]] = "llvm.mul"(%[[N2]], %[[ONE]]) : (i64, i64) -> i64
// CHECK: %[[ROW_OFF_B:[0-9]+]] = "llvm.mul"(%[[ZERO2]], %[[STRIDE0_B]]) : (i64, i64) -> i64
// CHECK: %[[LINEAR0_B:[0-9]+]] = "llvm.add"(%[[ROW_OFF_B]], %[[ZERO2]]) : (i64, i64) -> i64
// CHECK: %[[STORE_PTR2:[0-9]+]] = "llvm.getelementptr"(%[[PTR2]], %[[LINEAR0_B]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: "llvm.store"(%[[VAL2]], %[[STORE_PTR2]]) : (f32, !llvm.ptr) -> ()
// CHECK: "llvm.return"() : () -> ()

// RUN: scair-opt %s --passes lower-dmemref-to-llvm,convert-func-to-llvm,convert-llvm-export-abi | filecheck %s

builtin.module {
  func.func @refined_rank1(
    %n_nat : !dtensor.nat,
    %buf : !d_memref.memref<[%n_nat], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %x = d_memref.load %buf[%c0] : !d_memref.memref<[%n_nat], f32> -> f32
    d_memref.store %x, %buf[%c0] : f32, !d_memref.memref<[%n_nat], f32>
    func.return
  }

  func.func @refined_rank2(
    %m_nat : !dtensor.nat,
    %n_nat : !dtensor.nat,
    %buf : !d_memref.memref<[%m_nat, %n_nat], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %x = d_memref.load %buf[%c0, %c0] : !d_memref.memref<[%m_nat, %n_nat], f32> -> f32
    d_memref.store %x, %buf[%c0, %c0] : f32, !d_memref.memref<[%m_nat, %n_nat], f32>
    func.return
  }
}

// CHECK-LABEL: llvm.func @refined_rank1(
// CHECK-SAME: %0: i64, %1: !llvm.ptr
// CHECK: llvm.load

// CHECK-LABEL: llvm.func @refined_rank2(
// CHECK-SAME: %0: i64, %1: i64, %2: !llvm.ptr
// CHECK: llvm.load

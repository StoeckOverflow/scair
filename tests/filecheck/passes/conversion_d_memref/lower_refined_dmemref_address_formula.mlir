// RUN: scair-opt %s -p lower-dmemref-to-llvm | filecheck %s --implicit-check-not=d_memref.load --implicit-check-not=d_memref.store --implicit-check-not=d_memref.reinterpret_cast

builtin.module {
  func.func @address_formula(%stride0 : index, %stride1 : index, %i : index, %j : index) -> f32 {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c7 = "arith.constant"() <{value = 7 : index}> : () -> index
    %d0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %flat_nat = "dtensor.nat.const"() <{value = 32 : i32}> : () -> !dtensor.nat
    %flat = d_memref.alloc : () -> !d_memref.memref<[%flat_nat], f32>
    %view = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%flat_nat], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %c7, strides: [%stride0, %stride1]>
    %v = d_memref.load %view[%i, %j] : !d_memref.memref<[%d0, %d1], f32, offset: %c7, strides: [%stride0, %stride1]> -> f32
    d_memref.dealloc %flat : !d_memref.memref<[%flat_nat], f32>
    func.return %v : f32
  }
}

// CHECK-LABEL: func.func @address_formula
// CHECK-SAME: %[[S0:[0-9]+]]: i64, %[[S1:[0-9]+]]: i64, %[[I:[0-9]+]]: i64, %[[J:[0-9]+]]: i64
// CHECK: %[[ZERO:[0-9]+]] = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK: %[[S0I:[0-9]+]] = "llvm.add"(%[[S0]], %[[ZERO]]) : (i64, i64) -> i64
// CHECK: %[[S1I:[0-9]+]] = "llvm.add"(%[[S1]], %[[ZERO]]) : (i64, i64) -> i64
// CHECK: %[[II:[0-9]+]] = "llvm.add"(%[[I]], %[[ZERO]]) : (i64, i64) -> i64
// CHECK: %[[JI:[0-9]+]] = "llvm.add"(%[[J]], %[[ZERO]]) : (i64, i64) -> i64
// CHECK: %[[OFF:[0-9]+]] = "llvm.mlir.constant"() <{value = 7}> : () -> i64
// CHECK: %[[I_S0:[0-9]+]] = "llvm.mul"(%[[II]], %[[S0I]]) : (i64, i64) -> i64
// CHECK: %[[J_S1:[0-9]+]] = "llvm.mul"(%[[JI]], %[[S1I]]) : (i64, i64) -> i64
// CHECK: %[[OFF_PLUS_I:[0-9]+]] = "llvm.add"(%[[OFF]], %[[I_S0]]) : (i64, i64) -> i64
// CHECK: %[[LINEAR:[0-9]+]] = "llvm.add"(%[[OFF_PLUS_I]], %[[J_S1]]) : (i64, i64) -> i64
// CHECK: %[[PTR:[0-9]+]] = "llvm.getelementptr"(%{{[0-9]+}}, %[[LINEAR]]) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %[[VAL:[0-9]+]] = llvm.load %[[PTR]] : !llvm.ptr -> f32
// CHECK: "llvm.return"(%[[VAL]]) : (f32) -> ()

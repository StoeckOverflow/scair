// RUN: scair-opt %s -p normalize-refined-dmemref | filecheck %s

builtin.module {
  func.func @semi_affine_layout_map(%stride0 : index, %stride1 : index) -> f32 {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %total = "arith.muli"(%c256, %stride0) : (index, index) -> index
    %flat_nat = "dtensor.index_to_nat"(%total) : (index) -> !dtensor.nat
    %flat = d_memref.alloc : () -> !d_memref.memref<[%flat_nat], f32>
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
    %buf = d_memref.reinterpret_cast %flat to
      offset: [%zero],
      sizes: [%c256, %c1024],
      strides: [%stride0, %stride1]
    : !d_memref.memref<[%flat_nat], f32> to !d_memref.memref<[%d0, %d1], f32>
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %result = d_affine.for %i = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c256) step 1 : i32 iter_args(%acc = %cst : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c1024) step 1 : i32 iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %buf[%i, %j] : !d_memref.memref<[%d0, %d1], f32> -> f32
        %sum = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %sum : (f32)
      }
      d_affine.yield %inner : (f32)
    }
    d_memref.dealloc %flat : !d_memref.memref<[%flat_nat], f32>
    func.return %result : f32
  }
}

// CHECK-LABEL: func.func @semi_affine_layout_map
// CHECK: %[[C256:.*]] = "arith.constant"() <{value = 256 : index}>
// CHECK: %[[TOTAL:.*]] = "arith.muli"(%[[C256]], %0) : (index, index) -> index
// CHECK: %[[FLATNAT:.*]] = "dtensor.index_to_nat"(%[[TOTAL]]) : (index) -> !dtensor.nat
// CHECK: %[[FLAT:.*]] = d_memref.alloc : () -> !d_memref.memref<[%[[FLATNAT]]], f32>
// CHECK: %[[C1024:.*]] = "arith.constant"() <{value = 1024 : index}>
// CHECK: %[[ZERO:.*]] = "arith.constant"() <{value = 0 : index}>
// CHECK: %[[D0:.*]] = "dtensor.nat.const"() <{value = 256 : i32}>
// CHECK: %[[D1:.*]] = "dtensor.nat.const"() <{value = 1024 : i32}>
// CHECK: %[[BUF:.*]] = d_memref.reinterpret_cast %[[FLAT]] to
// CHECK:   offset: [%[[ZERO]]],
// CHECK:   sizes: [%[[C256]], %[[C1024]]],
// CHECK:   strides: [%0, %1]
// CHECK: : !d_memref.memref<[%[[FLATNAT]]], f32> to !d_memref.memref<[%[[D0]], %[[D1]]], f32, offset: 0 : index, strides: [%0, %1]>
// CHECK: d_memref.load %[[BUF]]

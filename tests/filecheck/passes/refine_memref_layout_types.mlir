// RUN: scair-opt %s -p refine-memref-layout-types | filecheck %s

builtin.module {
  func.func @refine(%stride0 : index, %stride1 : index, %off0 : index, %off1 : index) {
    %d0 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
    %d0_i = "dtensor.shape.to_index"(%d0) : (!dtensor.nat) -> index
    %d1_i = "dtensor.shape.to_index"(%d1) : (!dtensor.nat) -> index
    %src = d_memref.alloc : () -> !d_memref.memref<[%d0, %d1], f32>
    %sv = d_memref.subview %src[%off0, %off1][%d0_i, %d1_i][%stride0, %stride1] : !d_memref.memref<[%d0, %d1], f32> -> !d_memref.memref<[%d0, %d1], f32>
    func.return
  }
}

// CHECK-LABEL: func.func @refine
// CHECK: %{{.*}} = "dtensor.shape.to_index"(%{{.*}}) : (!dtensor.nat) -> index
// CHECK: %[[OFF0:.*]] = "arith.muli"(%2, %{{.*}}) : (index, index) -> index
// CHECK: %[[OFF:.*]] = "arith.addi"(%[[OFF0]], %3) : (index, index) -> index
// CHECK: %{{.*}} = "dtensor.shape.to_index"(%{{.*}}) : (!dtensor.nat) -> index
// CHECK: %[[STR0:.*]] = "arith.muli"(%0, %{{.*}}) : (index, index) -> index
// CHECK: %[[SV:.*]] = d_memref.subview %{{.*}}[%2, %3][%{{.*}}, %{{.*}}][%0, %1] : !d_memref.memref<[%{{.*}}, %{{.*}}], f32> -> !d_memref.memref<[%{{.*}}, %{{.*}}], f32, offset: %[[OFF]], strides: [%[[STR0]], %1]>

builtin.module {
  func.func @refine_reinterpret(%stride0 : index, %stride1 : index) {
    %d0 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 16 : i32}> : () -> !dtensor.nat
    %d0_i = "dtensor.shape.to_index"(%d0) : (!dtensor.nat) -> index
    %d1_i = "dtensor.shape.to_index"(%d1) : (!dtensor.nat) -> index
    %flat = "dtensor.nat.const"() <{value = 128 : i32}> : () -> !dtensor.nat
    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %src = d_memref.alloc : () -> !d_memref.memref<[%flat], f32>
    %rc = d_memref.reinterpret_cast %src to
      offset: [%zero],
      sizes: [%d0_i, %d1_i],
      strides: [%stride0, %stride1]
    : !d_memref.memref<[%flat], f32> to !d_memref.memref<[%d0, %d1], f32>
    func.return
  }
}

// CHECK-LABEL: func.func @refine_reinterpret
// CHECK: %[[RC:.*]] = d_memref.reinterpret_cast %{{.*}} to
// CHECK: offset: [%{{.*}}],
// CHECK: sizes: [%{{.*}}, %{{.*}}],
// CHECK: strides: [%0, %1]
// CHECK: : !d_memref.memref<[%{{.*}}], f32> to !d_memref.memref<[%{{.*}}, %{{.*}}], f32, offset: %{{.*}}, strides: [%0, %1]>

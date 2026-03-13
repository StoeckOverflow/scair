// RUN: scair-opt %s --allow-unregistered-dialect -p refine-memref-layout-types,canonicalize-dependent-layouts | filecheck %s

builtin.module {
  func.func @pipeline(%stride0 : index, %stride1 : index) {
    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
    %d0_i = "dtensor.shape.to_index"(%d0) : (!dtensor.nat) -> index
    %d1_i = "dtensor.shape.to_index"(%d1) : (!dtensor.nat) -> index
    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %src = d_memref.alloc : () -> !d_memref.memref<[%d0, %d1], f32>
    %sv = d_memref.subview %src[%zero, %zero][%d0_i, %d1_i][%stride0, %stride1] : !d_memref.memref<[%d0, %d1], f32> -> !d_memref.memref<[%d0, %d1], f32>
    %v = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
    d_memref.store %v, %sv[%zero, %zero] : f32, !d_memref.memref<[%d0, %d1], f32>
    %r = d_memref.load %sv[%zero, %zero] : !d_memref.memref<[%d0, %d1], f32> -> f32
    "test.keep"(%r) : (f32) -> ()
    func.return
  }
}

// CHECK-LABEL: func.func @pipeline
// CHECK: %[[S0:.*]] = "arith.muli"(%0, %{{.*}}) : (index, index) -> index
// CHECK: %[[SV:.*]] = d_memref.subview %{{.*}}[%{{.*}}, %{{.*}}][%{{.*}}, %{{.*}}][%0, %1] : !d_memref.memref<[%{{.*}}, %{{.*}}], f32> -> !d_memref.memref<[%{{.*}}, %{{.*}}], f32, offset: 0 : index, strides: [%[[S0]], %1]>
// CHECK: d_memref.store %{{.*}}, %[[SV]][%{{.*}}, %{{.*}}] : f32, !d_memref.memref<[%{{.*}}, %{{.*}}], f32, offset: 0 : index, strides: [%[[S0]], %1]>
// CHECK: d_memref.load %[[SV]][%{{.*}}, %{{.*}}] : !d_memref.memref<[%{{.*}}, %{{.*}}], f32, offset: 0 : index, strides: [%[[S0]], %1]> -> f32

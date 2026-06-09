// RUN: scair-opt %s -p normalize-refined-layout-accesses | filecheck %s

builtin.module {
  func.func @normalize(%stride0 : index, %stride1 : index, %i : index, %j : index) -> f32 {
    %d0 = "d_tensor.nat.const"() <{value = 256 : i32}> : () -> !d_tensor.nat
    %d1 = "d_tensor.nat.const"() <{value = 1024 : i32}> : () -> !d_tensor.nat
    %flat = "d_tensor.nat.const"() <{value = 262144 : i32}> : () -> !d_tensor.nat
    %buf = d_memref.alloc : () -> !d_memref.memref<[%flat], f32>
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %view = d_memref.reinterpret_cast %buf
    : !d_memref.memref<[%flat], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]>
    %v = d_memref.load %view[%i, %j] : !d_memref.memref<[%d0, %d1], f32, offset: %c0, strides: [%stride0, %stride1]> -> f32
    func.return %v : f32
  }

  func.func @normalize_store(%stride0 : index, %stride1 : index, %i : index, %j : index, %v : f32) {
    %d0 = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
    %d1 = "d_tensor.nat.const"() <{value = 16 : i32}> : () -> !d_tensor.nat
    %flat = "d_tensor.nat.const"() <{value = 128 : i32}> : () -> !d_tensor.nat
    %buf = d_memref.alloc : () -> !d_memref.memref<[%flat], f32>
    %c3 = "arith.constant"() <{value = 3 : index}> : () -> index
    %view = d_memref.reinterpret_cast %buf
    : !d_memref.memref<[%flat], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %c3, strides: [%stride0, %stride1]>
    d_memref.store %v, %view[%i, %j] : f32, !d_memref.memref<[%d0, %d1], f32, offset: %c3, strides: [%stride0, %stride1]>
    func.return
  }

  func.func @normalize_subview_load(%i : index) -> f32 {
    %d0 = "d_tensor.nat.const"() <{value = 16 : i32}> : () -> !d_tensor.nat
    %d1 = "d_tensor.nat.const"() <{value = 8 : i32}> : () -> !d_tensor.nat
    %flat = "d_tensor.nat.const"() <{value = 16 : i32}> : () -> !d_tensor.nat
    %src = d_memref.alloc : () -> !d_memref.memref<[%d0], f32>
    %off = "arith.constant"() <{value = 4 : index}> : () -> index
    %size = "d_tensor.shape.to_index"(%d1) : (!d_tensor.nat) -> index
    %stride = "arith.constant"() <{value = 2 : index}> : () -> index
    %sv = d_memref.subview %src[%off][%size][%stride] : !d_memref.memref<[%d0], f32> -> !d_memref.memref<[%d1], f32>
    %v = d_memref.load %sv[%i] : !d_memref.memref<[%d1], f32> -> f32
    func.return %v : f32
  }
}

// CHECK-LABEL: func.func @normalize(%0: index, %1: index, %2: index, %3: index) -> f32 {
// CHECK-NEXT:    %4 = "d_tensor.nat.const"() <{value = 256 : i32}> : () -> !d_tensor.nat
// CHECK-NEXT:    %5 = "d_tensor.nat.const"() <{value = 1024 : i32}> : () -> !d_tensor.nat
// CHECK-NEXT:    %6 = "d_tensor.nat.const"() <{value = 262144 : i32}> : () -> !d_tensor.nat
// CHECK-NEXT:    %7 = d_memref.alloc : () -> !d_memref.memref<[%6], f32>
// CHECK-NEXT:    %8 = "arith.constant"() <{value = 256 : index}> : () -> index
// CHECK-NEXT:    %9 = "arith.constant"() <{value = 1024 : index}> : () -> index
// CHECK-NEXT:    %10 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %11 = d_memref.reinterpret_cast %7
// CHECK-NEXT:    : !d_memref.memref<[%6], f32> to !d_memref.memref<[%4, %5], f32, offset: %10, strides: [%0, %1]>
// CHECK-NEXT:    %12 = "arith.muli"(%2, %0) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %13 = "arith.muli"(%3, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %14 = "arith.addi"(%10, %12) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %15 = "arith.addi"(%14, %13) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %16 = d_memref.load %7[%15] : !d_memref.memref<[%6], f32> -> f32
// CHECK-NEXT:    func.return %16 : f32
// CHECK-NEXT:  }
// CHECK-LABEL: func.func @normalize_store(%0: index, %1: index, %2: index, %3: index, %4: f32) {
// CHECK:        %[[BUF:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%{{[0-9]+}}], f32>
// CHECK:        %[[OFF:[0-9]+]] = "arith.constant"() <{value = 3 : index}> : () -> index
// CHECK:        %[[M0:[0-9]+]] = "arith.muli"(%2, %0) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK:        %[[M1:[0-9]+]] = "arith.muli"(%3, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK:        %[[A0:[0-9]+]] = "arith.addi"(%[[OFF]], %[[M0]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK:        %[[LIN:[0-9]+]] = "arith.addi"(%[[A0]], %[[M1]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK:        d_memref.store %4, %[[BUF]][%[[LIN]]] : f32, !d_memref.memref<[%{{[0-9]+}}], f32>
// CHECK:        func.return
// CHECK:      }
// CHECK-LABEL: func.func @normalize_subview_load(%0: index) -> f32 {
// CHECK:        %[[SRC:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%{{[0-9]+}}], f32>
// CHECK:        %[[OFF:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK:        %[[STRIDE:[0-9]+]] = "arith.constant"() <{value = 2 : index}> : () -> index
// CHECK:        %[[SCALED:[0-9]+]] = "arith.muli"(%0, %[[STRIDE]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK:        %[[SHIFTED:[0-9]+]] = "arith.addi"(%[[SCALED]], %[[OFF]]) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK:        %[[V:[0-9]+]] = d_memref.load %[[SRC]][%[[SHIFTED]]] : !d_memref.memref<[%{{[0-9]+}}], f32> -> f32
// CHECK:        func.return %[[V]] : f32
// CHECK:      }

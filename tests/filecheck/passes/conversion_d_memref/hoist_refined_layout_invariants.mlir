// RUN: scair-opt %s -p normalize-refined-layout-accesses,d-affine-loop-invariant-code-motion | filecheck %s

#map = affine_map<(d0)[] -> (d0)>

builtin.module {
  func.func @hoist(%stride0 : index, %stride1 : index) -> f32 {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %total = "arith.muli"(%c256, %stride0) : (index, index) -> index
    %flat = d_memref.alloc : () -> !d_memref.memref<[%total], f32>
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %buf = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%total], f32> to !d_memref.memref<[%c256, %c1024], f32, offset: %c0, strides: [%stride0, %stride1]>
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %result = d_affine.for %i = #map(%c0) to #map(%c256) step 1 : i32 iter_args(%acc = %cst : f32) {
      %inner = d_affine.for %j = #map(%c0) to #map(%c1024) step 1 : i32 iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %buf[%i, %j] : !d_memref.memref<[%c256, %c1024], f32, offset: %c0, strides: [%stride0, %stride1]> -> f32
        %sum = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %sum : (f32)
      }
      d_affine.yield %inner : (f32)
    }
    func.return %result : f32
  }
}

// CHECK-LABEL: func.func @hoist(%0: index, %1: index) -> f32 {
// CHECK-NEXT:    %2 = "arith.constant"() <{value = 256 : index}> : () -> index
// CHECK-NEXT:    %3 = "arith.muli"(%2, %0) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %4 = d_memref.alloc : () -> !d_memref.memref<[%3], f32>
// CHECK-NEXT:    %5 = "arith.constant"() <{value = 1024 : index}> : () -> index
// CHECK-NEXT:    %6 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %7 = d_memref.reinterpret_cast %4
// CHECK-NEXT:    : !d_memref.memref<[%3], f32> to !d_memref.memref<[%2, %5], f32, offset: %6, strides: [%0, %1]>
// CHECK-NEXT:    %8 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// CHECK-NEXT:    %9 = d_affine.for %10 = #map(%6) to #map(%2) step 1 : i32 iter_args(%11 = %8 : f32) {
// CHECK-NEXT:      %12 = "arith.muli"(%10, %0) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:      %13 = "arith.addi"(%6, %12) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:      %14 = d_affine.for %15 = #map(%6) to #map(%5) step 1 : i32 iter_args(%16 = %11 : f32) {
// CHECK-NEXT:        %17 = "arith.muli"(%15, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:        %18 = "arith.addi"(%13, %17) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:        %19 = d_memref.load %4[%18] : !d_memref.memref<[%3], f32> -> f32
// CHECK-NEXT:        %20 = "arith.addf"(%16, %19) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
// CHECK-NEXT:        d_affine.yield %20 : (f32)
// CHECK-NEXT:      }
// CHECK-NEXT:      d_affine.yield %14 : (f32)
// CHECK-NEXT:    }
// CHECK-NEXT:    func.return %9 : f32
// CHECK-NEXT:  }

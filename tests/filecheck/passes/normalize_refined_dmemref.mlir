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

// CHECK-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// CHECK-NEXT:    %2 = "arith.constant"() <{value = 256 : index}> : () -> index
// CHECK-NEXT:    %3 = "arith.muli"(%2, %0) : (index, index) -> index
// CHECK-NEXT:    %4 = "dtensor.index_to_nat"(%3) : (index) -> !dtensor.nat
// CHECK-NEXT:    %5 = d_memref.alloc : () -> !d_memref.memref<[%4], f32>
// CHECK-NEXT:    %6 = "arith.constant"() <{value = 1024 : index}> : () -> index
// CHECK-NEXT:    %7 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %8 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %9 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %10 = d_memref.reinterpret_cast %5 to
// CHECK-NEXT:      offset: [%7],
// CHECK-NEXT:      sizes: [%2, %6],
// CHECK-NEXT:      strides: [%0, %1]
// CHECK-NEXT:    : !d_memref.memref<[%4], f32> to !d_memref.memref<[%8, %9], f32, offset: 0 : index, strides: [%0, %1]>
// CHECK-NEXT:    %11 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
// CHECK-NEXT:    %12 = d_affine.for %13 = #map(%7) to #map(%2) step 1 : i32 iter_args(%14 = %11 : f32) {
// CHECK-NEXT:      %15 = d_affine.for %16 = #map(%7) to #map(%6) step 1 : i32 iter_args(%17 = %14 : f32) {
// CHECK-NEXT:        %18 = d_memref.load %10[%13, %16] : !d_memref.memref<[%8, %9], f32, offset: 0 : index, strides: [%0, %1]> -> f32
// CHECK-NEXT:        %19 = "arith.addf"(%17, %18) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
// CHECK-NEXT:        d_affine.yield %19 : (f32)
// CHECK-NEXT:      }
// CHECK-NEXT:      d_affine.yield %15 : (f32)
// CHECK-NEXT:    }
// CHECK-NEXT:    d_memref.dealloc %5 : !d_memref.memref<[%4], f32>
// CHECK-NEXT:    func.return %12 : f32
// CHECK-NEXT:  }

// RUN: scair-opt %s -p refine-dynamic-layout-to-d-memref | filecheck %s

builtin.module {
  func.func @refine_baseline(%stride0 : index, %stride1 : index) -> f32 {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %total = "arith.muli"(%c256, %stride0) : (index, index) -> index
    %flat = "memref.alloc"(%total) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> memref<?xf32>
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %buf = "memref.reinterpret_cast"(%flat, %c0, %c256, %c1024, %stride0, %stride1) <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}> : (memref<?xf32>, index, index, index, index, index) -> memref<256x1024xf32, strided<[?, ?], offset: 0>>
    %v = "memref.load"(%buf, %c0, %c0) : (memref<256x1024xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
    memref.dealloc %flat : memref<?xf32>
    func.return %v : f32
  }
}

// CHECK-LABEL: func.func @refine_baseline(%0: index, %1: index) -> f32 {
// CHECK-NEXT:    %2 = "arith.constant"() <{value = 256 : index}> : () -> index
// CHECK-NEXT:    %3 = "arith.muli"(%2, %0) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
// CHECK-NEXT:    %4 = d_memref.alloc : () -> !d_memref.memref<[%3], f32>
// CHECK-NEXT:    %5 = "arith.constant"() <{value = 1024 : index}> : () -> index
// CHECK-NEXT:    %6 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %7 = d_memref.reinterpret_cast %4
// CHECK-NEXT:    : !d_memref.memref<[%3], f32> to !d_memref.memref<[256 : index, 1024 : index], f32, offset: %6, strides: [%0, %1]>
// CHECK-NEXT:    %8 = d_memref.load %7[%6, %6] : !d_memref.memref<[256 : index, 1024 : index], f32, offset: %6, strides: [%0, %1]> -> f32
// CHECK-NEXT:    d_memref.dealloc %4 : !d_memref.memref<[%3], f32>
// CHECK-NEXT:    func.return %8 : f32
// CHECK-NEXT:  }

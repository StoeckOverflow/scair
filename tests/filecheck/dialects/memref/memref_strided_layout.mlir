// RUN: scair-opt %s | scair-opt | filecheck %s

builtin.module {
  func.func @baseline_layout(%stride0 : index, %stride1 : index) {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %flat = "memref.alloc"(%c256) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> memref<?xf32>
    %view = memref.reinterpret_cast %flat to
      offset: [%c0],
      sizes: [%c256, %c1024],
      strides: [%stride0, %stride1]
    : memref<?xf32> to memref<256x1024xf32, strided<[?, ?], offset: 0>>
    memref.dealloc %flat : memref<?xf32>
    func.return
  }
}

// CHECK-LABEL: func.func @baseline_layout(%0: index, %1: index) {
// CHECK-NEXT:    %2 = "arith.constant"() <{value = 256 : index}> : () -> index
// CHECK-NEXT:    %3 = "arith.constant"() <{value = 1024 : index}> : () -> index
// CHECK-NEXT:    %4 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %5 = "memref.alloc"(%2) <{operandSegmentSizes = array<i32: 1, 0>, alignment = 0}> : (index) -> memref<?xf32>
// CHECK-NOT:     operandSegmentSizes = array<i32: 1, 1, 2, 2>
// CHECK-NEXT:    %6 = memref.reinterpret_cast %5 to
// CHECK-NEXT:      offset: [%4],
// CHECK-NEXT:      sizes: [%2, %3],
// CHECK-NEXT:      strides: [%0, %1]
// CHECK-NEXT:    : memref<?xf32> to memref<256x1024xf32, strided<[?, ?], offset: 0>>
// CHECK-NEXT:    memref.dealloc %5 : memref<?xf32>
// CHECK-NEXT:    func.return
// CHECK-NEXT:  }

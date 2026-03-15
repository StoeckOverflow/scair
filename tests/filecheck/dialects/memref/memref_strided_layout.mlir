// RUN: scair-opt %s | filecheck %s

builtin.module {
  func.func @baseline_layout(%stride0 : index, %stride1 : index) {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %flat = "memref.alloc"(%c256) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> memref<?xf32>
    %view = "memref.reinterpret_cast"(%flat, %c0, %c256, %c1024, %stride0, %stride1) <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}> : (memref<?xf32>, index, index, index, index, index) -> memref<256x1024xf32, strided<[?, ?], offset: 0>>
    memref.dealloc %flat : memref<?xf32>
    func.return
  }
}

// CHECK-LABEL: func.func @baseline_layout(%0: index, %1: index) {
// CHECK-NEXT:    %2 = "arith.constant"() <{value = 256 : index}> : () -> index
// CHECK-NEXT:    %3 = "arith.constant"() <{value = 1024 : index}> : () -> index
// CHECK-NEXT:    %4 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %5 = "memref.alloc"(%2) <{alignment = 0, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> memref<?xf32>
// CHECK-NEXT:    %6 = "memref.reinterpret_cast"(%5, %4, %2, %3, %0, %1) <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}> : (memref<?xf32>, index, index, index, index, index) -> memref<256x1024xf32, strided<[?, ?], offset: 0>>
// CHECK-NEXT:    memref.dealloc %5 : memref<?xf32>
// CHECK-NEXT:    func.return
// CHECK-NEXT:  }

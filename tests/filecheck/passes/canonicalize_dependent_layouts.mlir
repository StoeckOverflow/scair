// RUN: scair-opt %s --allow-unregistered-dialect -p canonicalize-dependent-layouts | filecheck %s

builtin.module {
  func.func @canon(%base : index, %stride : index, %other : index) {
    %d0 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %one = "arith.constant"() <{value = 1 : index}> : () -> index
    %base0 = "arith.addi"(%base, %zero) : (index, index) -> index
    %stride0 = "arith.muli"(%stride, %one) : (index, index) -> index
    %view = "test.layout"() : () -> !d_memref.memref<[%d0], i32, offset: %base0, strides: [%stride0]>
    "test.keep"(%view, %other) : (!d_memref.memref<[%d0], i32, offset: %base0, strides: [%stride0]>, index) -> ()
    func.return
  }
}

// CHECK-LABEL: func.func @canon(%0: index, %1: index, %2: index) {
// CHECK-NEXT:    %3 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
// CHECK-NEXT:    %4 = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %5 = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK-NEXT:    %6 = "arith.addi"(%0, %4) : (index, index) -> index
// CHECK-NEXT:    %7 = "arith.muli"(%1, %5) : (index, index) -> index
// CHECK-NEXT:    %8 = "test.layout"() : () -> !d_memref.memref<[%3], i32, offset: %0, strides: [%1]>
// CHECK-NEXT:    "test.keep"(%8, %2) : (!d_memref.memref<[%3], i32, offset: %0, strides: [%1]>, index) -> ()
// CHECK-NEXT:    func.return
// CHECK-NEXT:  }

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-tensor-to-d-memref-shape-preserving | filecheck %s -DFILE=%s

builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %t = "test.tensor"() : () -> !d_tensor.tensor<[%m], i32>
  %d = "d_tensor.dim"(%t) <{axis = 0 : i32}>
    : (!d_tensor.tensor<[%m], i32>) -> !value<%m>
  "test.keep"(%d) : (!value<%m>) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "test.tensor"() : () -> !d_tensor.tensor<[%0], i32>
// CHECK-NEXT:   %2 = "builtin.unrealized_conversion_cast"(%1) : (!d_tensor.tensor<[%0], i32>) -> !d_memref.memref<[%0], i32>
// CHECK-NEXT:   %3 = d_memref.dim_exact %2 {axis = 0 : i32} : !d_memref.memref<[%0], i32> -> !value<%0>
// CHECK-NEXT:   "test.keep"(%3) : (!value<%0>) -> ()
// CHECK-NEXT: }

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-to-dmemref-shape-preserving | filecheck %s -DFILE=%s

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %t = "test.tensor"() : () -> !dtensor.tensor<[%m], i32>
  %d = "dtensor.dim"(%t) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m], i32>) -> !value<%m>
  "test.keep"(%d) : (!value<%m>) -> ()
}

// CHECK: %2 = "builtin.unrealized_conversion_cast"(%1) : (!dtensor.tensor<[%0], i32>) -> !d_memref.memref<[%0], i32>
// CHECK: %3 = d_memref.dim %2 {axis = 0 : i32} : !d_memref.memref<[%0], i32> -> !value<%0>

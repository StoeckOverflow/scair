// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-to-dmemref-shape-preserving | filecheck %s -DFILE=%s

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %t = "test.tensor"() : () -> !dtensor.tensor<[%m], i32>
  %d = "dtensor.dim"(%t) <{axis = 0 : i32}>
    : (!dtensor.tensor<[%m], i32>) -> !value<%m>
  "test.keep"(%d) : (!value<%m>) -> ()
}

// CHECK: %[[M:.*]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %[[CAST:.*]] = "builtin.unrealized_conversion_cast"(%{{.*}}) : (!dtensor.tensor<[%[[M]]], i32>) -> !d_memref.memref<[%[[M]]], i32>
// CHECK: %[[D:.*]] = d_memref.dim_exact %[[CAST]] {axis = 0 : i32} : !d_memref.memref<[%[[M]]], i32> -> !value<%[[M]]>

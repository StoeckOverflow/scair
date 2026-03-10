// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref | filecheck %s -DFILE=%s

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %K = "dtensor.nat.param"() : () -> !dtensor.nat
  %A = "test.tensor_a"() : () -> !dtensor.tensor<[%m, %K], i32>
  %B = "test.tensor_b"() : () -> !dtensor.tensor<[%K, %n], i32>
  %C = "dtensor.matmul"(%A, %B)
       : (!dtensor.tensor<[%m, %K], i32>, !dtensor.tensor<[%K, %n], i32>)
       -> !dtensor.tensor<[%m, %n], i32>
  "test.keep"(%C) : (!dtensor.tensor<[%m, %n], i32>) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %10 = "builtin.unrealized_conversion_cast"(%3) : (!dtensor.tensor<[%0, %2], i32>) -> !d_memref.memref<[%0, %2], i32>
// CHECK: %11 = "builtin.unrealized_conversion_cast"(%4) : (!dtensor.tensor<[%2, %1], i32>) -> !d_memref.memref<[%2, %1], i32>
// CHECK: %12 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK: d_affine.for %14 = #map(%8) to #map(%9) step 1 : i32 {
// CHECK: %30 = "builtin.unrealized_conversion_cast"(%12) {tile.m.mode = "untiled_fallback", tile.n.value = 1 : i32, tile.m.value = 1 : i32, tile.k.mode = "untiled_fallback", tile.k.value = 1 : i32, tile.n.mode = "untiled_fallback"} : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>

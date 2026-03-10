// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref | filecheck %s -DFILE=%s

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %kTiles = "dtensor.nat.param"() : () -> !dtensor.nat
  %tk = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
  %K = "dtensor.nat.mul"(%kTiles, %tk) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %A = "test.tensor_a"() : () -> !dtensor.tensor<[%m, %K], i32>
  %B = "test.tensor_b"() : () -> !dtensor.tensor<[%K, %n], i32>
  %C = "dtensor.matmul"(%A, %B)
       : (!dtensor.tensor<[%m, %K], i32>, !dtensor.tensor<[%K, %n], i32>)
       -> !dtensor.tensor<[%m, %n], i32>
  "test.keep"(%C) : (!dtensor.tensor<[%m, %n], i32>) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: %3 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// CHECK: %4 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK: %14 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// CHECK: %16 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK: d_affine.for %18 = #map(%10) to #map(%11) step 1 : i32 {
// CHECK-NOT: d_affine.min
// CHECK: %34 = "builtin.unrealized_conversion_cast"(%16) {tile.m.mode = "untiled_fallback", tile.n.value = 1 : i32, tile.m.value = 1 : i32, tile.k.mode = "tail_free_tiled", tile.k.value = 64 : i32, tile.n.mode = "untiled_fallback"} : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>

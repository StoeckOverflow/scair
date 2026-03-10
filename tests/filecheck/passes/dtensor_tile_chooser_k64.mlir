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

// CHECK: d_affine.for
// CHECK-NOT: d_affine.min
// CHECK-DAG: tile.m.mode = "untiled_fallback"
// CHECK-DAG: tile.n.mode = "untiled_fallback"
// CHECK-DAG: tile.k.mode = "tail_free_tiled"
// CHECK-DAG: tile.m.value = 1 : i32
// CHECK-DAG: tile.n.value = 1 : i32
// CHECK-DAG: tile.k.value = 64 : i32

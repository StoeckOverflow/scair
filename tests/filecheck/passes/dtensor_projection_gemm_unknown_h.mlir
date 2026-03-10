// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref | filecheck %s -DFILE=%s

// CASE B (baseline-like comparator): H is unknown symbolic.
// Without a divisibility proof for H, tiling must conservatively fall back to untiled mode.
// A : [BS, H], W : [H, H], O : [BS, H]
builtin.module {
  %bs = "dtensor.nat.param"() : () -> !dtensor.nat
  %H = "dtensor.nat.param"() : () -> !dtensor.nat

  %A = "test.proj_input"() : () -> !dtensor.tensor<[%bs, %H], i32>
  %W = "test.proj_weight"() : () -> !dtensor.tensor<[%H, %H], i32>
  %O = "dtensor.matmul"(%A, %W)
    : (!dtensor.tensor<[%bs, %H], i32>, !dtensor.tensor<[%H, %H], i32>)
    -> !dtensor.tensor<[%bs, %H], i32>
  "test.keep"(%O) : (!dtensor.tensor<[%bs, %H], i32>) -> ()
}

// CHECK-LABEL: builtin.module
// CHECK-NOT: "dtensor.nat.mul"
// CHECK: d_memref.alloc : () -> !d_memref.memref<[%{{.*}}, %{{.*}}], i32>
// CHECK: d_affine.for
// CHECK: d_memref.subview
// CHECK-NOT: d_affine.min
// CHECK-DAG: tile.m.mode = "untiled_fallback"
// CHECK-DAG: tile.n.mode = "untiled_fallback"
// CHECK-DAG: tile.k.mode = "untiled_fallback"
// CHECK-DAG: tile.m.value = 1 : i32
// CHECK-DAG: tile.n.value = 1 : i32
// CHECK-DAG: tile.k.value = 1 : i32

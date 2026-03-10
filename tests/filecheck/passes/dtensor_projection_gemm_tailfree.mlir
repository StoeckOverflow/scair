// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref | filecheck %s -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref,d-memref-bounds-check | filecheck %s -DFILE=%s --check-prefix=BOUNDS

// CASE A (value-dependent): Transformer-style projection GEMM.
// Shape fact:
//   H = nh * 64
// Since 64 divides H by construction, K-tiling can be emitted tail-free.
// A : [BS, H], W : [H, H], O : [BS, H]
builtin.module {
  %bs = "dtensor.nat.param"() : () -> !dtensor.nat
  %nh = "dtensor.nat.param"() : () -> !dtensor.nat
  %k64 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
  %H = "dtensor.nat.mul"(%nh, %k64) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

  %A = "test.proj_input"() : () -> !dtensor.tensor<[%bs, %H], i32>
  %W = "test.proj_weight"() : () -> !dtensor.tensor<[%H, %H], i32>
  %O = "dtensor.matmul"(%A, %W)
    : (!dtensor.tensor<[%bs, %H], i32>, !dtensor.tensor<[%H, %H], i32>)
    -> !dtensor.tensor<[%bs, %H], i32>
  "test.keep"(%O) : (!dtensor.tensor<[%bs, %H], i32>) -> ()
}

// CHECK-LABEL: builtin.module
// CHECK: "dtensor.nat.mul"
// CHECK: d_memref.alloc : () -> !d_memref.memref<[%{{.*}}, %{{.*}}], i32>
// CHECK: d_affine.for
// CHECK: d_memref.subview
// CHECK-NOT: d_affine.min
// CHECK-DAG: tile.m.mode = "untiled_fallback"
// CHECK-DAG: tile.n.mode = "tail_free_tiled"
// CHECK-DAG: tile.k.mode = "tail_free_tiled"
// CHECK-DAG: tile.m.value = 1 : i32
// CHECK-DAG: tile.n.value = 64 : i32
// CHECK-DAG: tile.k.value = 64 : i32

// BOUNDS: d_memref.subview
// BOUNDS: tile.k.mode = "tail_free_tiled"

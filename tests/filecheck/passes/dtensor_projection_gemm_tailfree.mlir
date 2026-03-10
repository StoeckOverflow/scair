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

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %2 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// CHECK: %3 = "dtensor.nat.mul"(%1, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK: %4 = "test.proj_input"() : () -> !dtensor.tensor<[%0, %3], i32>
// CHECK: %5 = "test.proj_weight"() : () -> !dtensor.tensor<[%3, %3], i32>
// CHECK: %6 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK: %7 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// CHECK: %8 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// CHECK: %11 = "builtin.unrealized_conversion_cast"(%4) : (!dtensor.tensor<[%0, %3], i32>) -> !d_memref.memref<[%0, %3], i32>
// CHECK: %12 = "builtin.unrealized_conversion_cast"(%5) : (!dtensor.tensor<[%3, %3], i32>) -> !d_memref.memref<[%3, %3], i32>
// CHECK: %13 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// CHECK: %14 = "dtensor.shape.to_index"(%13) : (!dtensor.nat) -> index
// CHECK: %15 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// CHECK: %16 = "dtensor.shape.to_index"(%15) : (!dtensor.nat) -> index
// CHECK: %17 = d_memref.alloc : () -> !d_memref.memref<[%0, %3], i32>
// CHECK: d_affine.for %19 = #map(%9) to #map(%10) step 1 : i32 {
// CHECK: %21 = d_memref.subview %17[%9, %20][%6, %14][%10, %10] : !d_memref.memref<[%0, %3], i32> -> !d_memref.memref<[%0, %13], i32>
// CHECK: %25 = d_memref.subview %11[%9, %24][%6, %16][%10, %10] : !d_memref.memref<[%0, %3], i32> -> !d_memref.memref<[%0, %15], i32>
// CHECK: %26 = d_memref.subview %12[%24, %20][%16, %14][%10, %10] : !d_memref.memref<[%3, %3], i32> -> !d_memref.memref<[%15, %13], i32>
// CHECK-NOT: d_affine.min
// CHECK: %35 = "builtin.unrealized_conversion_cast"(%17) {tile.m.mode = "untiled_fallback", tile.n.value = 64 : i32, tile.m.value = 1 : i32, tile.k.mode = "tail_free_tiled", tile.k.value = 64 : i32, tile.n.mode = "tail_free_tiled"} : (!d_memref.memref<[%0, %3], i32>) -> !dtensor.tensor<[%0, %3], i32>

// BOUNDS: d_memref.subview
// BOUNDS: tile.k.mode = "tail_free_tiled"

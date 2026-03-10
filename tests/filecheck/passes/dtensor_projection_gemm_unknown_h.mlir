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

// CHECK-LABEL: builtin.module {
// CHECK: %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %2 = "test.proj_input"() : () -> !dtensor.tensor<[%0, %1], i32>
// CHECK: %3 = "test.proj_weight"() : () -> !dtensor.tensor<[%1, %1], i32>
// CHECK: %4 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK: %5 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK: %6 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK: %9 = "builtin.unrealized_conversion_cast"(%2) : (!dtensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// CHECK: %10 = "builtin.unrealized_conversion_cast"(%3) : (!dtensor.tensor<[%1, %1], i32>) -> !d_memref.memref<[%1, %1], i32>
// CHECK: %11 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK: d_affine.for %13 = #map(%7) to #map(%8) step 1 : i32 {
// CHECK: %15 = d_memref.subview %11[%7, %7][%4, %6][%8, %8] : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// CHECK: %19 = d_memref.subview %9[%7, %7][%4, %5][%8, %8] : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// CHECK: %20 = d_memref.subview %10[%7, %7][%5, %6][%8, %8] : !d_memref.memref<[%1, %1], i32> -> !d_memref.memref<[%1, %1], i32>
// CHECK-NOT: d_affine.min
// CHECK-NOT: "dtensor.nat.mul"
// CHECK: %29 = "builtin.unrealized_conversion_cast"(%11) {tile.m.mode = "untiled_fallback", tile.n.value = 1 : i32, tile.m.value = 1 : i32, tile.k.mode = "untiled_fallback", tile.k.value = 1 : i32, tile.n.mode = "untiled_fallback"} : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>

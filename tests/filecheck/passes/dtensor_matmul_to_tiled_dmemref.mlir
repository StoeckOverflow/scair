// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref | filecheck %s -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref,d-memref-bounds-check | filecheck %s -DFILE=%s --check-prefix=BOUNDS

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
// CHECK: %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK: %3 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// CHECK: %4 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK: %5 = "test.tensor_a"() : () -> !dtensor.tensor<[%0, %4], i32>
// CHECK: %6 = "test.tensor_b"() : () -> !dtensor.tensor<[%4, %1], i32>
// CHECK: %7 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// CHECK: %8 = "dtensor.shape.to_index"(%4) : (!dtensor.nat) -> index
// CHECK: %9 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// CHECK: %12 = "builtin.unrealized_conversion_cast"(%5) : (!dtensor.tensor<[%0, %4], i32>) -> !d_memref.memref<[%0, %4], i32>
// CHECK: %13 = "builtin.unrealized_conversion_cast"(%6) : (!dtensor.tensor<[%4, %1], i32>) -> !d_memref.memref<[%4, %1], i32>
// CHECK: %14 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
// CHECK: %15 = "dtensor.shape.to_index"(%14) : (!dtensor.nat) -> index
// CHECK: %16 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// CHECK: d_affine.for %18 = #map(%10) to #map(%11) step 1 : i32 {
// CHECK: %20 = d_memref.subview %16[%10, %10][%7, %9][%11, %11] : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// CHECK: %24 = d_memref.subview %12[%10, %23][%7, %15][%11, %11] : !d_memref.memref<[%0, %4], i32> -> !d_memref.memref<[%0, %14], i32>
// CHECK: %25 = d_memref.subview %13[%23, %10][%15, %9][%11, %11] : !d_memref.memref<[%4, %1], i32> -> !d_memref.memref<[%14, %1], i32>
// CHECK: %29 = d_memref.load %24[%26, %28] : !d_memref.memref<[%0, %14], i32> -> i32
// CHECK: %30 = d_memref.load %25[%28, %27] : !d_memref.memref<[%14, %1], i32> -> i32
// CHECK: %31 = d_memref.load %20[%26, %27] : !d_memref.memref<[%0, %1], i32> -> i32
// CHECK: %32 = "arith.muli"(%29, %30) : (i32, i32) -> i32
// CHECK: %33 = "arith.addi"(%31, %32) : (i32, i32) -> i32
// CHECK: d_memref.store %33, %20[%26, %27] : i32, !d_memref.memref<[%0, %1], i32>
// CHECK: %34 = "builtin.unrealized_conversion_cast"(%16) {tile.m.mode = "untiled_fallback", tile.n.value = 1 : i32, tile.m.value = 1 : i32, tile.k.mode = "tail_free_tiled", tile.k.value = 64 : i32, tile.n.mode = "untiled_fallback"} : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>
// CHECK-NOT: d_affine.min
// CHECK-NOT: "dtensor.matmul"

// BOUNDS: "builtin.unrealized_conversion_cast"
// BOUNDS: tile.k.mode = "tail_free_tiled"

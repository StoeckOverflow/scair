// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref,d-memref-bounds-check | filecheck %s -DFILE=%s --check-prefix=LOWER
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref | scair-run | filecheck %s -DFILE=%s --check-prefix=EXEC

// End-to-end executable test for the current specialized matmul lowering path:
//   dtensor.matmul
//     -> dtensor-matmul-to-tiled-dmemref
//     -> d_affine / d_memref
//     -> scair-run
//
// This is not a general dtensor -> d_memref pipeline milestone.
// The executable path here is specialized: inputs are materialized as d_memref
// buffers, cast to dtensor, lowered through the dedicated matmul pass, then
// executed on the lowered d_affine/d_memref form.
//
// Real-world-inspired kernel: transformer projection GEMM.
//   A : [BS, H]
//   W : [H, H]
//   O : [BS, H]
// Shape fact:
//   H = nh * 4
// Since 4 divides H by construction, N/K axes can be lowered tail-free.

builtin.module {
  func.func @main() -> i32 {
    %bs = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
    %nh = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
    %k4 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    %H = "dtensor.nat.mul"(%nh, %k4) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

    %zero_nat = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
    %zero = "dtensor.shape.to_index"(%zero_nat) : (!dtensor.nat) -> index
    %one = "arith.constant"() <{value = 1 : index}> : () -> index
    %two = "arith.constant"() <{value = 2 : index}> : () -> index
    %three = "arith.constant"() <{value = 3 : index}> : () -> index

    %A = d_memref.alloc : () -> !d_memref.memref<[%bs, %H], i32>
    %W = d_memref.alloc : () -> !d_memref.memref<[%H, %H], i32>

    %c1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
    %c2 = "arith.constant"() <{value = 2 : i32}> : () -> i32
    %c3 = "arith.constant"() <{value = 3 : i32}> : () -> i32
    %c4 = "arith.constant"() <{value = 4 : i32}> : () -> i32
    %c5 = "arith.constant"() <{value = 5 : i32}> : () -> i32
    %c6 = "arith.constant"() <{value = 6 : i32}> : () -> i32
    %c7 = "arith.constant"() <{value = 7 : i32}> : () -> i32
    %c8 = "arith.constant"() <{value = 8 : i32}> : () -> i32
    %c9 = "arith.constant"() <{value = 9 : i32}> : () -> i32
    %c10 = "arith.constant"() <{value = 10 : i32}> : () -> i32
    %c11 = "arith.constant"() <{value = 11 : i32}> : () -> i32
    %c12 = "arith.constant"() <{value = 12 : i32}> : () -> i32
    %c13 = "arith.constant"() <{value = 13 : i32}> : () -> i32
    %c14 = "arith.constant"() <{value = 14 : i32}> : () -> i32
    %c15 = "arith.constant"() <{value = 15 : i32}> : () -> i32
    %c16 = "arith.constant"() <{value = 16 : i32}> : () -> i32

    d_memref.store %c1, %A[%zero, %zero] : i32, !d_memref.memref<[%bs, %H], i32>
    d_memref.store %c2, %A[%zero, %one] : i32, !d_memref.memref<[%bs, %H], i32>
    d_memref.store %c3, %A[%zero, %two] : i32, !d_memref.memref<[%bs, %H], i32>
    d_memref.store %c4, %A[%zero, %three] : i32, !d_memref.memref<[%bs, %H], i32>

    d_memref.store %c1, %W[%zero, %zero] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c2, %W[%zero, %one] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c3, %W[%zero, %two] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c4, %W[%zero, %three] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c5, %W[%one, %zero] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c6, %W[%one, %one] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c7, %W[%one, %two] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c8, %W[%one, %three] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c9, %W[%two, %zero] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c10, %W[%two, %one] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c11, %W[%two, %two] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c12, %W[%two, %three] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c13, %W[%three, %zero] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c14, %W[%three, %one] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c15, %W[%three, %two] : i32, !d_memref.memref<[%H, %H], i32>
    d_memref.store %c16, %W[%three, %three] : i32, !d_memref.memref<[%H, %H], i32>

    %At = "builtin.unrealized_conversion_cast"(%A)
      : (!d_memref.memref<[%bs, %H], i32>) -> !dtensor.tensor<[%bs, %H], i32>
    %Wt = "builtin.unrealized_conversion_cast"(%W)
      : (!d_memref.memref<[%H, %H], i32>) -> !dtensor.tensor<[%H, %H], i32>

    %Ot = "dtensor.matmul"(%At, %Wt)
      : (!dtensor.tensor<[%bs, %H], i32>, !dtensor.tensor<[%H, %H], i32>)
      -> !dtensor.tensor<[%bs, %H], i32>
    %O = "builtin.unrealized_conversion_cast"(%Ot)
      : (!dtensor.tensor<[%bs, %H], i32>) -> !d_memref.memref<[%bs, %H], i32>

    %r = d_memref.load %O[%zero, %three] : !d_memref.memref<[%bs, %H], i32> -> i32
    func.return %r : i32
  }
}

// LOWER-LABEL: builtin.module {
// LOWER: func.func @main() -> i32 {
// LOWER: %0 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// LOWER: %1 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// LOWER: %2 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// LOWER: %3 = "dtensor.nat.mul"(%1, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// LOWER: %9 = d_memref.alloc : () -> !d_memref.memref<[%0, %3], i32>
// LOWER: %10 = d_memref.alloc : () -> !d_memref.memref<[%3, %3], i32>
// LOWER: %27 = "builtin.unrealized_conversion_cast"(%9) : (!d_memref.memref<[%0, %3], i32>) -> !dtensor.tensor<[%0, %3], i32>
// LOWER: %28 = "builtin.unrealized_conversion_cast"(%10) : (!d_memref.memref<[%3, %3], i32>) -> !dtensor.tensor<[%3, %3], i32>
// LOWER: %29 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// LOWER: %30 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// LOWER: %31 = "dtensor.shape.to_index"(%3) : (!dtensor.nat) -> index
// LOWER: %32 = "arith.constant"() <{value = 0 : index}> : () -> index
// LOWER: %33 = "arith.constant"() <{value = 1 : index}> : () -> index
// LOWER: %34 = "builtin.unrealized_conversion_cast"(%27) : (!dtensor.tensor<[%0, %3], i32>) -> !d_memref.memref<[%0, %3], i32>
// LOWER: %35 = "builtin.unrealized_conversion_cast"(%28) : (!dtensor.tensor<[%3, %3], i32>) -> !d_memref.memref<[%3, %3], i32>
// LOWER: %36 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// LOWER: %37 = "dtensor.shape.to_index"(%36) : (!dtensor.nat) -> index
// LOWER: %38 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// LOWER: %39 = "dtensor.shape.to_index"(%38) : (!dtensor.nat) -> index
// LOWER: %40 = d_memref.alloc : () -> !d_memref.memref<[%0, %3], i32>
// LOWER: d_affine.for %42 = #map(%32) to #map(%33) step 1 : i32 {
// LOWER: %44 = d_memref.subview %40[%32, %43][%29, %37][%33, %33] : !d_memref.memref<[%0, %3], i32> -> !d_memref.memref<[%0, %36], i32>
// LOWER: %48 = d_memref.subview %34[%32, %47][%29, %39][%33, %33] : !d_memref.memref<[%0, %3], i32> -> !d_memref.memref<[%0, %38], i32>
// LOWER: %49 = d_memref.subview %35[%47, %43][%39, %37][%33, %33] : !d_memref.memref<[%3, %3], i32> -> !d_memref.memref<[%38, %36], i32>
// LOWER: %53 = d_memref.load %48[%50, %52] : !d_memref.memref<[%0, %38], i32> -> i32
// LOWER: %54 = d_memref.load %49[%52, %51] : !d_memref.memref<[%38, %36], i32> -> i32
// LOWER: %55 = d_memref.load %44[%50, %51] : !d_memref.memref<[%0, %36], i32> -> i32
// LOWER: %56 = "arith.muli"(%53, %54) : (i32, i32) -> i32
// LOWER: %57 = "arith.addi"(%55, %56) : (i32, i32) -> i32
// LOWER: d_memref.store %57, %44[%50, %51] : i32, !d_memref.memref<[%0, %36], i32>
// LOWER: %58 = "builtin.unrealized_conversion_cast"(%40) {tile.m.mode = "untiled_fallback", tile.n.value = 4 : i32, tile.m.value = 1 : i32, tile.k.mode = "tail_free_tiled", tile.k.value = 4 : i32, tile.n.mode = "tail_free_tiled"} : (!d_memref.memref<[%0, %3], i32>) -> !dtensor.tensor<[%0, %3], i32>
// LOWER: %59 = "builtin.unrealized_conversion_cast"(%58) : (!dtensor.tensor<[%0, %3], i32>) -> !d_memref.memref<[%0, %3], i32>
// LOWER: %60 = d_memref.load %59[%5, %8] : !d_memref.memref<[%0, %3], i32> -> i32
// LOWER: func.return %60 : i32
// LOWER-NOT: "dtensor.matmul"
// LOWER-NOT: d_affine.min

// EXEC: Result: 120

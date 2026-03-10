// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref | filecheck %s -DFILE=%s --check-prefix=LOWER
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-matmul-to-tiled-dmemref | scair-run | filecheck %s -DFILE=%s --check-prefix=EXEC

builtin.module {
  func.func @main() -> i32 {
    %two = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
    %zero_nat = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
    %zero = "dtensor.shape.to_index"(%zero_nat) : (!dtensor.nat) -> index
    %one = "arith.constant"() <{value = 1 : index}> : () -> index

    %A = d_memref.alloc : () -> !d_memref.memref<[%two, %two], i32>
    %B = d_memref.alloc : () -> !d_memref.memref<[%two, %two], i32>

    %c1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
    %c2 = "arith.constant"() <{value = 2 : i32}> : () -> i32
    %c3 = "arith.constant"() <{value = 3 : i32}> : () -> i32
    %c4 = "arith.constant"() <{value = 4 : i32}> : () -> i32
    %c5 = "arith.constant"() <{value = 5 : i32}> : () -> i32
    %c6 = "arith.constant"() <{value = 6 : i32}> : () -> i32
    %c7 = "arith.constant"() <{value = 7 : i32}> : () -> i32
    %c8 = "arith.constant"() <{value = 8 : i32}> : () -> i32

    d_memref.store %c1, %A[%zero, %zero] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c2, %A[%zero, %one] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c3, %A[%one, %zero] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c4, %A[%one, %one] : i32, !d_memref.memref<[%two, %two], i32>

    d_memref.store %c5, %B[%zero, %zero] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c6, %B[%zero, %one] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c7, %B[%one, %zero] : i32, !d_memref.memref<[%two, %two], i32>
    d_memref.store %c8, %B[%one, %one] : i32, !d_memref.memref<[%two, %two], i32>

    %At = "builtin.unrealized_conversion_cast"(%A)
      : (!d_memref.memref<[%two, %two], i32>) -> !dtensor.tensor<[%two, %two], i32>
    %Bt = "builtin.unrealized_conversion_cast"(%B)
      : (!d_memref.memref<[%two, %two], i32>) -> !dtensor.tensor<[%two, %two], i32>

    %Ct = "dtensor.matmul"(%At, %Bt)
      : (!dtensor.tensor<[%two, %two], i32>, !dtensor.tensor<[%two, %two], i32>)
      -> !dtensor.tensor<[%two, %two], i32>
    %C = "builtin.unrealized_conversion_cast"(%Ct)
      : (!dtensor.tensor<[%two, %two], i32>) -> !d_memref.memref<[%two, %two], i32>

    %r = d_memref.load %C[%zero, %zero] : !d_memref.memref<[%two, %two], i32> -> i32
    func.return %r : i32
  }
}

// LOWER-LABEL: builtin.module {
// LOWER: func.func @main() -> i32 {
// LOWER: %0 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
// LOWER: %1 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// LOWER: %2 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// LOWER: %3 = "arith.constant"() <{value = 1 : index}> : () -> index
// LOWER: %4 = d_memref.alloc : () -> !d_memref.memref<[%0, %0], i32>
// LOWER: %5 = d_memref.alloc : () -> !d_memref.memref<[%0, %0], i32>
// LOWER: %14 = "builtin.unrealized_conversion_cast"(%4) : (!d_memref.memref<[%0, %0], i32>) -> !dtensor.tensor<[%0, %0], i32>
// LOWER: %15 = "builtin.unrealized_conversion_cast"(%5) : (!d_memref.memref<[%0, %0], i32>) -> !dtensor.tensor<[%0, %0], i32>
// LOWER: %16 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// LOWER: %17 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// LOWER: %18 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// LOWER: %19 = "arith.constant"() <{value = 0 : index}> : () -> index
// LOWER: %20 = "arith.constant"() <{value = 1 : index}> : () -> index
// LOWER: %21 = "builtin.unrealized_conversion_cast"(%14) : (!dtensor.tensor<[%0, %0], i32>) -> !d_memref.memref<[%0, %0], i32>
// LOWER: %22 = "builtin.unrealized_conversion_cast"(%15) : (!dtensor.tensor<[%0, %0], i32>) -> !d_memref.memref<[%0, %0], i32>
// LOWER: %23 = d_memref.alloc : () -> !d_memref.memref<[%0, %0], i32>
// LOWER: d_affine.for %25 = #map(%19) to #map(%20) step 1 : i32 {
// LOWER: %27 = d_memref.subview %23[%19, %19][%16, %18][%20, %20] : !d_memref.memref<[%0, %0], i32> -> !d_memref.memref<[%0, %0], i32>
// LOWER: %31 = d_memref.subview %21[%19, %19][%16, %17][%20, %20] : !d_memref.memref<[%0, %0], i32> -> !d_memref.memref<[%0, %0], i32>
// LOWER: %32 = d_memref.subview %22[%19, %19][%17, %18][%20, %20] : !d_memref.memref<[%0, %0], i32> -> !d_memref.memref<[%0, %0], i32>
// LOWER: %36 = d_memref.load %31[%33, %35] : !d_memref.memref<[%0, %0], i32> -> i32
// LOWER: %37 = d_memref.load %32[%35, %34] : !d_memref.memref<[%0, %0], i32> -> i32
// LOWER: %38 = d_memref.load %27[%33, %34] : !d_memref.memref<[%0, %0], i32> -> i32
// LOWER: %39 = "arith.muli"(%36, %37) : (i32, i32) -> i32
// LOWER: %40 = "arith.addi"(%38, %39) : (i32, i32) -> i32
// LOWER: d_memref.store %40, %27[%33, %34] : i32, !d_memref.memref<[%0, %0], i32>
// LOWER: %41 = "builtin.unrealized_conversion_cast"(%23) {tile.m.mode = "untiled_fallback", tile.n.value = 1 : i32, tile.m.value = 1 : i32, tile.k.mode = "untiled_fallback", tile.k.value = 1 : i32, tile.n.mode = "untiled_fallback"} : (!d_memref.memref<[%0, %0], i32>) -> !dtensor.tensor<[%0, %0], i32>
// LOWER: %42 = "builtin.unrealized_conversion_cast"(%41) : (!dtensor.tensor<[%0, %0], i32>) -> !d_memref.memref<[%0, %0], i32>
// LOWER: %43 = d_memref.load %42[%2, %2] : !d_memref.memref<[%0, %0], i32> -> i32
// LOWER: func.return %43 : i32
// LOWER-NOT: "dtensor.matmul"

// EXEC: Result: 19

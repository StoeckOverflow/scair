// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-tensor-to-d-memref-shape-preserving,reconcile-unrealized-casts | filecheck %s -DFILE=%s --check-prefix=LOWER
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-tensor-to-d-memref-shape-preserving,reconcile-unrealized-casts | scair-run | filecheck %s -DFILE=%s --check-prefix=EXEC

// Bridge-only executable test for the current minimal d_tensor -> d_memref subset.
// This exercises d_tensor.fill, d_tensor.cast, and d_tensor.dim through lowering.
// Execution still happens on the lowered d_memref form, not on direct d_tensor runtime semantics.

builtin.module {
  func.func @main() -> i32 {
    %n = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    %seven = "arith.constant"() <{value = 7 : i32}> : () -> i32
    %one = "arith.constant"() <{value = 1 : index}> : () -> index

    %t0 = "d_tensor.fill"(%seven) : (i32) -> !d_tensor.tensor<[%n], i32>
    %d = "d_tensor.dim"(%t0) <{axis = 0 : i32}> : (!d_tensor.tensor<[%n], i32>) -> !value<%n>
    %t1 = "d_tensor.cast"(%t0) : (!d_tensor.tensor<[%n], i32>) -> !d_tensor.tensor<[%n], i32>
    %m = "builtin.unrealized_conversion_cast"(%t1) : (!d_tensor.tensor<[%n], i32>) -> !d_memref.memref<[%n], i32>

    %r = d_memref.load %m[%one] : !d_memref.memref<[%n], i32> -> i32
    func.return %r : i32
  }
}

// LOWER-LABEL: builtin.module {
// LOWER: func.func @main() -> i32 {
// LOWER: %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// LOWER: %1 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// LOWER: %2 = "arith.constant"() <{value = 1 : index}> : () -> index
// LOWER: %3 = "arith.constant"() <{value = 0 : index}> : () -> index
// LOWER: %4 = "d_tensor.shape.to_index"(%0) : (!d_tensor.nat) -> index
// LOWER: %5 = d_memref.alloc : () -> !d_memref.memref<[%0], i32>
// LOWER: d_affine.for %6 = #map(%3) to #map(%4) step 1 : i32 {
// LOWER: d_memref.store %1, %5[%6] : i32, !d_memref.memref<[%0], i32>
// LOWER: d_affine.yield
// LOWER: }
// LOWER: %7 = d_memref.dim_exact %5 {axis = 0 : i32} : !d_memref.memref<[%0], i32> -> !value<%0>
// LOWER: %8 = d_memref.cast %5 : !d_memref.memref<[%0], i32> -> !d_memref.memref<[%0], i32>
// LOWER: %9 = d_memref.load %8[%2] : !d_memref.memref<[%0], i32> -> i32
// LOWER: func.return %9 : i32
// LOWER-NOT: "d_tensor.fill"
// LOWER-NOT: "d_tensor.cast"
// LOWER-NOT: "d_tensor.dim"

// EXEC: Result: 7

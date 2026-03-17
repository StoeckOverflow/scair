// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-to-dmemref-shape-preserving | filecheck %s -DFILE=%s --check-prefix=LOWER
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p dtensor-to-dmemref-shape-preserving,reconcile-unrealized-casts | filecheck %s -DFILE=%s --check-prefix=RECON

// Minimal executable dtensor -> d_memref bridge subset.
// This pass now lowers:
//   - dtensor.empty
//   - dtensor.fill
//   - dtensor.cast
//   - dtensor.dim
// It is still not a full general dtensor conversion pipeline.

builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "arith.constant"() <{value = 7 : i32}> : () -> i32

  %e = "dtensor.empty"() : () -> !dtensor.tensor<[%m, %n], i32>
  %f = "dtensor.fill"(%z) : (i32) -> !dtensor.tensor<[%m, %n], i32>
  %c = "dtensor.cast"(%f) : (!dtensor.tensor<[%m, %n], i32>) -> !dtensor.tensor<[%m, %n], i32>
  %d = "dtensor.dim"(%c) <{axis = 1 : i32}> : (!dtensor.tensor<[%m, %n], i32>) -> !value<%n>

  "test.keep"(%e, %c, %d) : (!dtensor.tensor<[%m, %n], i32>, !dtensor.tensor<[%m, %n], i32>, !value<%n>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER-NEXT:   %2 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// LOWER-NEXT:   %3 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   %4 = "builtin.unrealized_conversion_cast"(%3) : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>
// LOWER-NEXT:   %5 = "arith.constant"() <{value = 0 : index}> : () -> index
// LOWER-NEXT:   %6 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// LOWER-NEXT:   %7 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// LOWER-NEXT:   %8 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   d_affine.for %9 = #map(%5) to #map(%6) step 1 : i32 {
// LOWER-NEXT:     d_affine.for %10 = #map(%5) to #map(%7) step 1 : i32 {
// LOWER-NEXT:       d_memref.store %2, %8[%9, %10] : i32, !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:       d_affine.yield
// LOWER-NEXT:     }
// LOWER-NEXT:     d_affine.yield
// LOWER-NEXT:   }
// LOWER-NEXT:   %11 = "builtin.unrealized_conversion_cast"(%8) : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>
// LOWER-NEXT:   %12 = "builtin.unrealized_conversion_cast"(%11) : (!dtensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   %13 = d_memref.cast %12 : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   %14 = "builtin.unrealized_conversion_cast"(%13) : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>
// LOWER-NEXT:   %15 = "builtin.unrealized_conversion_cast"(%14) : (!dtensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   %16 = d_memref.dim_exact %15 {axis = 1 : i32} : !d_memref.memref<[%0, %1], i32> -> !value<%1>
// LOWER-NEXT:   "test.keep"(%4, %14, %16) : (!dtensor.tensor<[%0, %1], i32>, !dtensor.tensor<[%0, %1], i32>, !value<%1>) -> ()
// LOWER-NEXT: }

// RECON-LABEL: builtin.module {
// RECON-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// RECON-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// RECON-NEXT:   %2 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// RECON-NEXT:   %3 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// RECON-NEXT:   %4 = "builtin.unrealized_conversion_cast"(%3) : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>
// RECON-NEXT:   %5 = "arith.constant"() <{value = 0 : index}> : () -> index
// RECON-NEXT:   %6 = "dtensor.shape.to_index"(%0) : (!dtensor.nat) -> index
// RECON-NEXT:   %7 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
// RECON-NEXT:   %8 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// RECON-NEXT:   d_affine.for %9 = #map(%5) to #map(%6) step 1 : i32 {
// RECON-NEXT:     d_affine.for %10 = #map(%5) to #map(%7) step 1 : i32 {
// RECON-NEXT:       d_memref.store %2, %8[%9, %10] : i32, !d_memref.memref<[%0, %1], i32>
// RECON-NEXT:       d_affine.yield
// RECON-NEXT:     }
// RECON-NEXT:     d_affine.yield
// RECON-NEXT:   }
// RECON-NEXT:   %11 = d_memref.cast %8 : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// RECON-NEXT:   %12 = "builtin.unrealized_conversion_cast"(%11) : (!d_memref.memref<[%0, %1], i32>) -> !dtensor.tensor<[%0, %1], i32>
// RECON-NEXT:   %13 = d_memref.dim_exact %11 {axis = 1 : i32} : !d_memref.memref<[%0, %1], i32> -> !value<%1>
// RECON-NEXT:   "test.keep"(%4, %12, %13) : (!dtensor.tensor<[%0, %1], i32>, !dtensor.tensor<[%0, %1], i32>, !value<%1>) -> ()
// RECON-NEXT: }

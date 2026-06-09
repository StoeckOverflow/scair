// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-tensor-to-d-memref-shape-preserving | filecheck %s -DFILE=%s --check-prefix=LOWER
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p d-tensor-to-d-memref-shape-preserving,reconcile-unrealized-casts | filecheck %s -DFILE=%s --check-prefix=RECON

// Minimal executable d_tensor -> d_memref bridge subset.
// This pass now lowers:
//   - d_tensor.empty
//   - d_tensor.fill
//   - d_tensor.cast
//   - d_tensor.dim
// It is still not a full general d_tensor conversion pipeline.

builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %z = "arith.constant"() <{value = 7 : i32}> : () -> i32

  %e = "d_tensor.empty"() : () -> !d_tensor.tensor<[%m, %n], i32>
  %f = "d_tensor.fill"(%z) : (i32) -> !d_tensor.tensor<[%m, %n], i32>
  %c = "d_tensor.cast"(%f) : (!d_tensor.tensor<[%m, %n], i32>) -> !d_tensor.tensor<[%m, %n], i32>
  %d = "d_tensor.dim"(%c) <{axis = 1 : i32}> : (!d_tensor.tensor<[%m, %n], i32>) -> !value<%n>

  "test.keep"(%e, %c, %d) : (!d_tensor.tensor<[%m, %n], i32>, !d_tensor.tensor<[%m, %n], i32>, !value<%n>) -> ()
}

// LOWER-LABEL: builtin.module {
// LOWER-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// LOWER-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// LOWER-NEXT:   %2 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// LOWER-NEXT:   %3 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   %4 = "builtin.unrealized_conversion_cast"(%3) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// LOWER-NEXT:   %5 = "arith.constant"() <{value = 0 : index}> : () -> index
// LOWER-NEXT:   %6 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   d_affine.for %7 = #map(%5) to #map(%0) step 1 : i32 {
// LOWER-NEXT:     d_affine.for %8 = #map(%5) to #map(%1) step 1 : i32 {
// LOWER-NEXT:       d_memref.store %2, %6[%7, %8] : i32, !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:       d_affine.yield
// LOWER-NEXT:     }
// LOWER-NEXT:     d_affine.yield
// LOWER-NEXT:   }
// LOWER-NEXT:   %9 = "builtin.unrealized_conversion_cast"(%6) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// LOWER-NEXT:   %10 = "builtin.unrealized_conversion_cast"(%9) : (!d_tensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   %11 = d_memref.cast %10 : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   %12 = "builtin.unrealized_conversion_cast"(%11) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// LOWER-NEXT:   %13 = "builtin.unrealized_conversion_cast"(%12) : (!d_tensor.tensor<[%0, %1], i32>) -> !d_memref.memref<[%0, %1], i32>
// LOWER-NEXT:   %14 = d_memref.dim_exact %13 {axis = 1 : i32} : !d_memref.memref<[%0, %1], i32> -> !value<%1>
// LOWER-NEXT:   "test.keep"(%4, %12, %14) : (!d_tensor.tensor<[%0, %1], i32>, !d_tensor.tensor<[%0, %1], i32>, !value<%1>) -> ()
// LOWER-NEXT: }

// RECON-LABEL: builtin.module {
// RECON-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// RECON-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// RECON-NEXT:   %2 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// RECON-NEXT:   %3 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// RECON-NEXT:   %4 = "builtin.unrealized_conversion_cast"(%3) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// RECON-NEXT:   %5 = "arith.constant"() <{value = 0 : index}> : () -> index
// RECON-NEXT:   %6 = d_memref.alloc : () -> !d_memref.memref<[%0, %1], i32>
// RECON-NEXT:   d_affine.for %7 = #map(%5) to #map(%0) step 1 : i32 {
// RECON-NEXT:     d_affine.for %8 = #map(%5) to #map(%1) step 1 : i32 {
// RECON-NEXT:       d_memref.store %2, %6[%7, %8] : i32, !d_memref.memref<[%0, %1], i32>
// RECON-NEXT:       d_affine.yield
// RECON-NEXT:     }
// RECON-NEXT:     d_affine.yield
// RECON-NEXT:   }
// RECON-NEXT:   %9 = d_memref.cast %6 : !d_memref.memref<[%0, %1], i32> -> !d_memref.memref<[%0, %1], i32>
// RECON-NEXT:   %10 = "builtin.unrealized_conversion_cast"(%9) : (!d_memref.memref<[%0, %1], i32>) -> !d_tensor.tensor<[%0, %1], i32>
// RECON-NEXT:   %11 = d_memref.dim_exact %9 {axis = 1 : i32} : !d_memref.memref<[%0, %1], i32> -> !value<%1>
// RECON-NEXT:   "test.keep"(%4, %10, %11) : (!d_tensor.tensor<[%0, %1], i32>, !d_tensor.tensor<[%0, %1], i32>, !value<%1>) -> ()
// RECON-NEXT: }

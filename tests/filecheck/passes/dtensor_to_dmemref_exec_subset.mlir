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

// LOWER-LABEL: builtin.module
// LOWER: %[[M:.*]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER: %[[N:.*]] = "dtensor.nat.param"() : () -> !dtensor.nat
// LOWER: %[[EMPTY_ALLOC:.*]] = d_memref.alloc : () -> !d_memref.memref<[%[[M]], %[[N]]], i32>
// LOWER: %[[I0:.*]] = "dtensor.shape.to_index"(%[[M]]) : (!dtensor.nat) -> index
// LOWER: %[[I1:.*]] = "dtensor.shape.to_index"(%[[N]]) : (!dtensor.nat) -> index
// LOWER: d_affine.for
// LOWER: d_memref.store
// LOWER: d_memref.cast
// LOWER: d_memref.dim_exact
// LOWER-NOT: "dtensor.empty"
// LOWER-NOT: "dtensor.fill"
// LOWER-NOT: "dtensor.cast"
// LOWER-NOT: "dtensor.dim"

// RECON-LABEL: builtin.module
// RECON: d_memref.alloc : () -> !d_memref.memref<[%{{.*}}, %{{.*}}], i32>
// RECON: d_memref.cast
// RECON-NOT: "builtin.unrealized_conversion_cast"(%{{.*}}) : (!d_memref.memref<[%{{.*}}, %{{.*}}], i32>) -> !d_memref.memref<[%{{.*}}, %{{.*}}], i32>

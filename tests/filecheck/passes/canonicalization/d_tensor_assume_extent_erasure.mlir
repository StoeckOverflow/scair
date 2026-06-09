// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE

// assume_extent is verification-only metadata and is erased by pure-op cleanup.
builtin.module {
  %n = "test.index"() : () -> index
  "d_tensor.assume_extent"(%n) : (index) -> ()
  %t = "test.t"() : () -> !d_tensor.tensor<[%n], f32>
  "test.keep"(%t) : (!d_tensor.tensor<[%n], f32>) -> ()
}

// CANON: builtin.module {
// CANON:   %0 = "test.index"() : () -> index
// CANON-NOT: d_tensor.assume_extent
// CANON:   %1 = "test.t"() : () -> !d_tensor.tensor<[%0], f32>
// CANON:   "test.keep"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// CANON: }

// DCE: builtin.module {
// DCE:   %0 = "test.index"() : () -> index
// DCE-NOT: d_tensor.assume_extent
// DCE:   %1 = "test.t"() : () -> !d_tensor.tensor<[%0], f32>
// DCE:   "test.keep"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// DCE: }

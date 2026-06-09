// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

// Shape canonicalization on symbolic dims; deep RAUW into type dims.
builtin.module {
  %p = "d_tensor.size.param"() : () -> !d_tensor.size
  %z = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %o = "d_tensor.size.constant"() <{value = 1 : i32}> : () -> !d_tensor.size
  %s = "d_tensor.size.add"(%p, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %m = "d_tensor.size.mul"(%s, %o) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
}

// CANON: builtin.module {
// CANON-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CANON-NEXT:   %1 = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
// CANON-NEXT:   %2 = "d_tensor.size.constant"() <{value = 1 : i32}> : () -> !d_tensor.size
// CANON-NEXT:   %3 = "test.use"() : () -> !d_tensor.tensor<[%0], f32>
// CANON-NEXT: }

// -----

// Must-not-fold case.
builtin.module {
  %p = "d_tensor.size.param"() : () -> !d_tensor.size
  %q = "d_tensor.size.param"() : () -> !d_tensor.size
  %s = "d_tensor.size.add"(%p, %q) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CANON: builtin.module {
// CANON:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CANON:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CANON:   %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CANON:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CANON: }

// -----

// CSE regression: result types with different dim SSA identity must not merge.
builtin.module {
  %x0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %x1 = "d_tensor.size.param"() : () -> !d_tensor.size
  %e0 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%x0], f32>
  %e1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%x1], f32>
  %v = "test.scalar"() : () -> f32
  %f0 = "d_tensor.fill"(%v) : (f32) -> !d_tensor.tensor<[%x0], f32>
  %f1 = "d_tensor.fill"(%v) : (f32) -> !d_tensor.tensor<[%x1], f32>
}

// CSE: builtin.module {
// CSE:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CSE:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CSE:   %2 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0], f32>
// CSE:   %3 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%1], f32>
// CSE:   %4 = "test.scalar"() : () -> f32
// CSE:   %5 = "d_tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%0], f32>
// CSE:   %6 = "d_tensor.fill"(%4) : (f32) -> !d_tensor.tensor<[%1], f32>
// CSE: }

// -----

// CSE within nested unregistered region ops: do not merge across islands.
builtin.module {
  "test.island_a"() ({
  ^bb0:
    %c1 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
    %c2 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
    %s = "d_tensor.size.add"(%c1, %c2) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    "test.yield"() : () -> ()
  }) : () -> ()
  "test.island_b"() ({
  ^bb0:
    %c1 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
    %c2 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
    %s = "d_tensor.size.add"(%c1, %c2) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    "test.yield"() : () -> ()
  }) : () -> ()
}

// CSE: builtin.module {
// CSE:   "test.island_a"() ({
// CSE:     %0 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
// CSE:     %1 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// CSE:     %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CSE:     "test.yield"() : () -> ()
// CSE:   }) : () -> ()
// CSE:   "test.island_b"() ({
// CSE:     %0 = "d_tensor.size.constant"() <{value = 2 : i32}> : () -> !d_tensor.size
// CSE:     %1 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
// CSE:     %2 = "d_tensor.size.add"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CSE:     "test.yield"() : () -> ()
// CSE:   }) : () -> ()
// CSE: }

// -----

// DCE regression: keep type-only dim uses across user chain.
builtin.module {
  %p = "d_tensor.size.param"() : () -> !d_tensor.size
  %t = "d_tensor.empty"() : () -> !d_tensor.tensor<[%p], f32>
  %u = "test.id"(%t) : (!d_tensor.tensor<[%p], f32>) -> !d_tensor.tensor<[%p], f32>
  "test.keep"(%u) : (!d_tensor.tensor<[%p], f32>) -> ()
}

// DCE: builtin.module {
// DCE:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// DCE:   %1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0], f32>
// DCE:   %2 = "test.id"(%1) : (!d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>
// DCE:   "test.keep"(%2) : (!d_tensor.tensor<[%0], f32>) -> ()
// DCE: }

// -----

// DCE regression: remove dead nat algebra.
builtin.module {
  %m = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
  %z = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %a = "d_tensor.size.add"(%m, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %b = "d_tensor.size.mul"(%a, %m) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %t = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
  "test.keep_dead"(%t) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// DCE: builtin.module {
// DCE:   %0 = "d_tensor.size.constant"() <{value = 4 : i32}> : () -> !d_tensor.size
// DCE:   %1 = "test.use"() : () -> !d_tensor.tensor<[%0], f32>
// DCE:   "test.keep_dead"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// DCE: }

// -----

// Full pipeline: fold and propagate symbolic dims.
builtin.module {
  %p = "d_tensor.size.param"() : () -> !d_tensor.size
  %z = "d_tensor.size.constant"() <{value = 0 : i32}> : () -> !d_tensor.size
  %s0 = "d_tensor.size.add"(%p, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %s1 = "d_tensor.size.add"(%p, %z) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %k0 = "test.keep"() : () -> !d_tensor.tensor<[%s0], f32>
  %k1 = "test.keep"() : () -> !d_tensor.tensor<[%s1], f32>
}

// PIPE: builtin.module {
// PIPE:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// PIPE:   %1 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE:   %2 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE: }

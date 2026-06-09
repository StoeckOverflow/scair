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
  %p = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %z = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %o = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
  %s = "d_tensor.nat.add"(%p, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %m = "d_tensor.nat.mul"(%s, %o) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %u = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
}

// CANON: builtin.module {
// CANON-NEXT:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %1 = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
// CANON-NEXT:   %2 = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
// CANON-NEXT:   %3 = "test.use"() : () -> !d_tensor.tensor<[%0], f32>
// CANON-NEXT: }

// -----

// Must-not-fold case.
builtin.module {
  %p = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %q = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %s = "d_tensor.nat.add"(%p, %q) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CANON: builtin.module {
// CANON:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %2 = "d_tensor.nat.add"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CANON:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CANON: }

// -----

// CSE regression: result types with different dim SSA identity must not merge.
builtin.module {
  %x0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %x1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %e0 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%x0], f32>
  %e1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%x1], f32>
  %v = "test.scalar"() : () -> f32
  %f0 = "d_tensor.fill"(%v) : (f32) -> !d_tensor.tensor<[%x0], f32>
  %f1 = "d_tensor.fill"(%v) : (f32) -> !d_tensor.tensor<[%x1], f32>
}

// CSE: builtin.module {
// CSE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CSE:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
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
    %c1 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
    %c2 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
    %s = "d_tensor.nat.add"(%c1, %c2) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    "test.yield"() : () -> ()
  }) : () -> ()
  "test.island_b"() ({
  ^bb0:
    %c1 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
    %c2 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
    %s = "d_tensor.nat.add"(%c1, %c2) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    "test.yield"() : () -> ()
  }) : () -> ()
}

// CSE: builtin.module {
// CSE:   "test.island_a"() ({
// CSE:     %0 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
// CSE:     %1 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
// CSE:     %2 = "d_tensor.nat.add"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CSE:     "test.yield"() : () -> ()
// CSE:   }) : () -> ()
// CSE:   "test.island_b"() ({
// CSE:     %0 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
// CSE:     %1 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
// CSE:     %2 = "d_tensor.nat.add"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CSE:     "test.yield"() : () -> ()
// CSE:   }) : () -> ()
// CSE: }

// -----

// DCE regression: keep type-only dim uses across user chain.
builtin.module {
  %p = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %t = "d_tensor.empty"() : () -> !d_tensor.tensor<[%p], f32>
  %u = "test.id"(%t) : (!d_tensor.tensor<[%p], f32>) -> !d_tensor.tensor<[%p], f32>
  "test.keep"(%u) : (!d_tensor.tensor<[%p], f32>) -> ()
}

// DCE: builtin.module {
// DCE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// DCE:   %1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0], f32>
// DCE:   %2 = "test.id"(%1) : (!d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>
// DCE:   "test.keep"(%2) : (!d_tensor.tensor<[%0], f32>) -> ()
// DCE: }

// -----

// DCE regression: remove dead nat algebra.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %z = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %a = "d_tensor.nat.add"(%m, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %b = "d_tensor.nat.mul"(%a, %m) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %t = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
  "test.keep_dead"(%t) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// DCE: builtin.module {
// DCE:   %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// DCE:   %1 = "test.use"() : () -> !d_tensor.tensor<[%0], f32>
// DCE:   "test.keep_dead"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// DCE: }

// -----

// Full pipeline: fold and propagate symbolic dims.
builtin.module {
  %p = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %z = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %s0 = "d_tensor.nat.add"(%p, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %s1 = "d_tensor.nat.add"(%p, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %k0 = "test.keep"() : () -> !d_tensor.tensor<[%s0], f32>
  %k1 = "test.keep"() : () -> !d_tensor.tensor<[%s1], f32>
}

// PIPE: builtin.module {
// PIPE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE:   %1 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE:   %2 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE: }

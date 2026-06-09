// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

// Shape canonicalization accepts symbolic index dims without Nat surface syntax.
builtin.module {
  %p = "test.index"() : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %o = "arith.constant"() <{value = 1 : index}> : () -> index
  %s = "arith.addi"(%p, %z) : (index, index) -> index
  %m = "arith.muli"(%s, %o) : (index, index) -> index
  %u = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
}

// CANON: builtin.module {
// CANON-NEXT:   %0 = "test.index"() : () -> index
// CANON-NEXT:   %1 = "arith.constant"() <{value = 0 : index}> : () -> index
// CANON-NEXT:   %2 = "arith.constant"() <{value = 1 : index}> : () -> index
// CANON-NEXT:   %3 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CANON-NEXT:   %4 = "arith.muli"(%3, %2) {{.*}} : (index, index) -> index
// CANON-NEXT:   %5 = "test.use"() : () -> !d_tensor.tensor<[%4], f32>
// CANON-NEXT: }

// -----

// Must-not-fold case.
builtin.module {
  %p = "test.index"() : () -> index
  %q = "test.index"() : () -> index
  %s = "arith.addi"(%p, %q) : (index, index) -> index
  %u = "test.use"() : () -> !d_tensor.tensor<[%s], f32>
}

// CANON: builtin.module {
// CANON:   %0 = "test.index"() : () -> index
// CANON:   %1 = "test.index"() : () -> index
// CANON:   %2 = "arith.addi"(%0, %1) {{.*}} : (index, index) -> index
// CANON:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CANON: }

// -----

// CSE regression: result types with different dim SSA identity must not merge.
builtin.module {
  %x0 = "test.index"() : () -> index
  %x1 = "test.index"() : () -> index
  %e0 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%x0], f32>
  %e1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%x1], f32>
  %v = "test.scalar"() : () -> f32
  %f0 = "d_tensor.fill"(%v) : (f32) -> !d_tensor.tensor<[%x0], f32>
  %f1 = "d_tensor.fill"(%v) : (f32) -> !d_tensor.tensor<[%x1], f32>
}

// CSE: builtin.module {
// CSE:   %0 = "test.index"() : () -> index
// CSE:   %1 = "test.index"() : () -> index
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
    %x = "test.index"() : () -> index
    %y = "test.index"() : () -> index
    "test.consume"(%x, %y) : (index, index) -> ()
    "test.yield"() : () -> ()
  }) : () -> ()
  "test.island_b"() ({
  ^bb0:
    %x = "test.index"() : () -> index
    %y = "test.index"() : () -> index
    "test.consume"(%x, %y) : (index, index) -> ()
    "test.yield"() : () -> ()
  }) : () -> ()
}

// CSE: builtin.module {
// CSE:   "test.island_a"() ({
// CSE:     %0 = "test.index"() : () -> index
// CSE:     %1 = "test.index"() : () -> index
// CSE:     "test.consume"(%0, %1) : (index, index) -> ()
// CSE:     "test.yield"() : () -> ()
// CSE:   }) : () -> ()
// CSE:   "test.island_b"() ({
// CSE:     %0 = "test.index"() : () -> index
// CSE:     %1 = "test.index"() : () -> index
// CSE:     "test.consume"(%0, %1) : (index, index) -> ()
// CSE:     "test.yield"() : () -> ()
// CSE:   }) : () -> ()
// CSE: }

// -----

// DCE regression: keep type-only dim uses across user chain.
builtin.module {
  %p = "test.index"() : () -> index
  %t = "d_tensor.empty"() : () -> !d_tensor.tensor<[%p], f32>
  %u = "test.id"(%t) : (!d_tensor.tensor<[%p], f32>) -> !d_tensor.tensor<[%p], f32>
  "test.keep"(%u) : (!d_tensor.tensor<[%p], f32>) -> ()
}

// DCE: builtin.module {
// DCE:   %0 = "test.index"() : () -> index
// DCE:   %1 = "d_tensor.empty"() : () -> !d_tensor.tensor<[%0], f32>
// DCE:   %2 = "test.id"(%1) : (!d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>
// DCE:   "test.keep"(%2) : (!d_tensor.tensor<[%0], f32>) -> ()
// DCE: }

// -----

// DCE regression: remove dead index arithmetic.
builtin.module {
  %m = "test.index"() : () -> index
  %z = "test.index"() : () -> index
  %a = "arith.addi"(%m, %z) : (index, index) -> index
  %b = "arith.muli"(%a, %z) : (index, index) -> index
  %t = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
  "test.keep_dead"(%t) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// DCE: builtin.module {
// DCE:   %0 = "test.index"() : () -> index
// DCE:   %1 = "test.index"() : () -> index
// DCE:   %2 = "test.use"() : () -> !d_tensor.tensor<[%0], f32>
// DCE:   "test.keep_dead"(%2) : (!d_tensor.tensor<[%0], f32>) -> ()
// DCE: }

// -----

// Full pipeline: fold and propagate symbolic dims.
builtin.module {
  %p = "test.index"() : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %s0 = "arith.addi"(%p, %z) : (index, index) -> index
  %s1 = "arith.addi"(%p, %z) : (index, index) -> index
  %k0 = "test.keep_pipe"() : () -> !d_tensor.tensor<[%s0], f32>
  %k1 = "test.keep_pipe"() : () -> !d_tensor.tensor<[%s1], f32>
}

// PIPE:   %0 = "test.index"() : () -> index
// PIPE:   %1 = "test.keep_pipe"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE:   %2 = "test.keep_pipe"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE: }

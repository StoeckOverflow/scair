// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

// Shape canonicalization on symbolic dims; deep RAUW into type dims.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %o = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%p, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %m = "dtensor.nat.mul"(%s, %o) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.use"() : () -> !dtensor.tensor<[%m], f32>
}

// CANON: %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NOT: "dtensor.nat.add"
// CANON-NOT: "dtensor.nat.mul"
// CANON: "test.use"() : () -> !dtensor.tensor<[%0], f32>

// -----

// Must-not-fold case.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %q = "dtensor.nat.param"() : () -> !dtensor.nat
  %s = "dtensor.nat.add"(%p, %q) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.use"() : () -> !dtensor.tensor<[%s], f32>
}

// CANON: "dtensor.nat.add"(%0, %1)
// CANON: "test.use"() : () -> !dtensor.tensor<[%2], f32>

// -----

// CSE regression: result types with different dim SSA identity must not merge.
builtin.module {
  %x0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %x1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %e0 = "dtensor.empty"() : () -> !dtensor.tensor<[%x0], f32>
  %e1 = "dtensor.empty"() : () -> !dtensor.tensor<[%x1], f32>
  %v = "test.scalar"() : () -> f32
  %f0 = "dtensor.fill"(%v) : (f32) -> !dtensor.tensor<[%x0], f32>
  %f1 = "dtensor.fill"(%v) : (f32) -> !dtensor.tensor<[%x1], f32>
}

// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[%1], f32>
// CSE: "dtensor.fill"(%4) : (f32) -> !dtensor.tensor<[%0], f32>
// CSE: "dtensor.fill"(%4) : (f32) -> !dtensor.tensor<[%1], f32>

// -----

// CSE within nested unregistered region ops: do not merge across islands.
builtin.module {
  "test.island_a"() ({
  ^bb0:
    %c1 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
    %c2 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %s = "dtensor.nat.add"(%c1, %c2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    "test.yield"() : () -> ()
  }) : () -> ()
  "test.island_b"() ({
  ^bb0:
    %c1 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
    %c2 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %s = "dtensor.nat.add"(%c1, %c2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    "test.yield"() : () -> ()
  }) : () -> ()
}

// CSE: "test.island_a"() ({
// CSE: "dtensor.nat.add"
// CSE: "test.island_b"() ({
// CSE: "dtensor.nat.add"

// -----

// DCE regression: keep type-only dim uses across user chain.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %t = "dtensor.empty"() : () -> !dtensor.tensor<[%p], f32>
  %u = "test.id"(%t) : (!dtensor.tensor<[%p], f32>) -> !dtensor.tensor<[%p], f32>
  "test.keep"(%u) : (!dtensor.tensor<[%p], f32>) -> ()
}

// DCE: "test.id"
// DCE: "test.keep"

// -----

// DCE regression: remove dead nat algebra.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %a = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "dtensor.nat.mul"(%a, %m) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %t = "test.use"() : () -> !dtensor.tensor<[%m], f32>
  "test.keep_dead"(%t) : (!dtensor.tensor<[%m], f32>) -> ()
}

// DCE: "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// DCE-NOT: "dtensor.nat.add"
// DCE-NOT: "dtensor.nat.mul"
// DCE: "test.keep_dead"

// -----

// Full pipeline: fold and propagate symbolic dims.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %s0 = "dtensor.nat.add"(%p, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s1 = "dtensor.nat.add"(%p, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %k0 = "test.keep"() : () -> !dtensor.tensor<[%s0], f32>
  %k1 = "test.keep"() : () -> !dtensor.tensor<[%s1], f32>
}

// PIPE: [[P:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE: "test.keep"() : () -> !dtensor.tensor<[[[P]]], f32>
// PIPE: "test.keep"() : () -> !dtensor.tensor<[[[P]]], f32>

// Purpose: nat.param-focused coverage for parse/print+verify, DCE uses-in-types, and CSE non-merge by dim SSA identity.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

// Parse/print + verify with nat.param in type params (tensor + sugar forms).
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %v = "test.v"() : () -> !d_tensor.vector<%m, f32>
  %mat = "test.mat"() : () -> !d_tensor.matrix<%m, %n, f32>
  %t = "test.t"() : () -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep_types"(%v, %mat, %t)
    : (!d_tensor.vector<%m, f32>, !d_tensor.matrix<%m, %n, f32>, !d_tensor.tensor<[%m, %n], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// VERIFY:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// VERIFY:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// VERIFY:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// VERIFY: }

// CANON: builtin.module {
// CANON:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// CANON:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// CANON:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CANON:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// CANON: }

// CSE: builtin.module {
// CSE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CSE:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CSE:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// CSE:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// CSE:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CSE:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// CSE: }

// DCE: builtin.module {
// DCE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// DCE:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// DCE:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// DCE:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// DCE:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// DCE:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// DCE: }

// PIPE: builtin.module {
// PIPE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// PIPE:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// PIPE:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// PIPE:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// PIPE: }

// -----

// CSE must not merge nat.param producers (fresh identity).
builtin.module {
  %p0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %p1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  "test.keep_params"(%p0, %p1) : (!d_tensor.nat, !d_tensor.nat) -> ()
}

// CSE: builtin.module {
// CSE:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CSE:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CSE:   "test.keep_params"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> ()
// CSE: }

// -----

// Dead nat.param should be removed by DCE.
builtin.module {
  %p = "d_tensor.nat.param"() : () -> !d_tensor.nat
}

// DCE: builtin.module {
// DCE: ^bb0:
// DCE: }

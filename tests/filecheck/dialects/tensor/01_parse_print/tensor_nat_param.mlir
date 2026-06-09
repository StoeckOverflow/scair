// Purpose: index-dimension coverage for parse/print+verify, DCE uses-in-types, and CSE non-merge by dim SSA identity.
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

// Parse/print + verify with index values in type params (tensor + sugar forms).
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %v = "test.v"() : () -> !d_tensor.vector<%m, f32>
  %mat = "test.mat"() : () -> !d_tensor.matrix<%m, %n, f32>
  %t = "test.t"() : () -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep_types"(%v, %mat, %t)
    : (!d_tensor.vector<%m, f32>, !d_tensor.matrix<%m, %n, f32>, !d_tensor.tensor<[%m, %n], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.index"() : () -> index
// VERIFY:   %1 = "test.index"() : () -> index
// VERIFY:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// VERIFY:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// VERIFY:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// VERIFY:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// VERIFY: }

// CANON: builtin.module {
// CANON:   %0 = "test.index"() : () -> index
// CANON:   %1 = "test.index"() : () -> index
// CANON:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// CANON:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// CANON:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CANON:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// CANON: }

// CSE: builtin.module {
// CSE:   %0 = "test.index"() : () -> index
// CSE:   %1 = "test.index"() : () -> index
// CSE:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// CSE:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// CSE:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CSE:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// CSE: }

// DCE: builtin.module {
// DCE:   %0 = "test.index"() : () -> index
// DCE:   %1 = "test.index"() : () -> index
// DCE:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// DCE:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// DCE:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// DCE:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// DCE: }

// PIPE: builtin.module {
// PIPE:   %0 = "test.index"() : () -> index
// PIPE:   %1 = "test.index"() : () -> index
// PIPE:   %2 = "test.v"() : () -> !d_tensor.vector<%0, f32>
// PIPE:   %3 = "test.mat"() : () -> !d_tensor.matrix<%0, %1, f32>
// PIPE:   %4 = "test.t"() : () -> !d_tensor.tensor<[%0, %1], f32>
// PIPE:   "test.keep_types"(%2, %3, %4) : (!d_tensor.vector<%0, f32>, !d_tensor.matrix<%0, %1, f32>, !d_tensor.tensor<[%0, %1], f32>) -> ()
// PIPE: }

// -----

// CSE must not merge opaque index producers (fresh identity).
builtin.module {
  %p0 = "test.index"() : () -> index
  %p1 = "test.index"() : () -> index
  "test.keep_params"(%p0, %p1) : (index, index) -> ()
}

// CSE: builtin.module {
// CSE:   %0 = "test.index"() : () -> index
// CSE:   %1 = "test.index"() : () -> index
// CSE:   "test.keep_params"(%0, %1) : (index, index) -> ()
// CSE: }

// -----

// Dead index constants should be removed by DCE.
builtin.module {
  %p = "arith.constant"() <{value = 7 : index}> : () -> index
}

// DCE: builtin.module {
// DCE: }

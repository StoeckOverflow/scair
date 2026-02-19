// Purpose: nat.param-focused coverage for parse/print+verify, DCE uses-in-types, and CSE non-merge by dim SSA identity.
// Coverage checklist (existing -> gap -> this file):
// - dtensor.nat.param exists in current symbolic tests -> add centralized nat.param-only invariants here.
// - parse/print with nat.param in vector/matrix/tensor types -> explicitly checked here.
// - DCE uses-in-types with nat.param-only dim producers -> explicitly checked here.
// - CSE non-merge when result types differ only by distinct nat.param SSA dims -> explicitly checked here.
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

// Parse/print + verify with nat.param in type params (tensor + sugar forms).
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %v = "test.v"() : () -> !dtensor.vector<%m, f32>
  %mat = "test.mat"() : () -> !dtensor.matrix<%m, %n, f32>
  %t = "test.t"() : () -> !dtensor.tensor<[%m, %n], f32>
  "test.keep_types"(%v, %mat, %t)
    : (!dtensor.vector<%m, f32>, !dtensor.matrix<%m, %n, f32>, !dtensor.tensor<[%m, %n], f32>) -> ()
}

// VERIFY: "dtensor.nat.param"()
// VERIFY: !dtensor.vector<%0, f32>
// VERIFY: !dtensor.matrix<%0, %1, f32>
// VERIFY: !dtensor.tensor<[%0, %1], f32>
// CANON: "test.keep_types"
// CSE: "test.keep_types"
// PIPE: "test.keep_types"

// -----

// nat.param must be preserved when only used via type params.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %t = "dtensor.empty"() : () -> !dtensor.tensor<[%p], f32>
  %u = "test.id"(%t) : (!dtensor.tensor<[%p], f32>) -> !dtensor.tensor<[%p], f32>
  "test.keep_dce_nat_param"(%u) : (!dtensor.tensor<[%p], f32>) -> ()
}

// DCE: dtensor.nat.param
// DCE: keep_dce_nat_param
// PIPE: "dtensor.nat.param"
// PIPE: "test.keep_dce_nat_param"

// -----

// Distinct nat.param dims mean result tensor types differ, so CSE must not merge.
builtin.module {
  %p0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %p1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %e0 = "dtensor.empty"() : () -> !dtensor.tensor<[%p0], f32>
  %e1 = "dtensor.empty"() : () -> !dtensor.tensor<[%p1], f32>
  "test.keep0"(%e0) : (!dtensor.tensor<[%p0], f32>) -> ()
  "test.keep1"(%e1) : (!dtensor.tensor<[%p1], f32>) -> ()
}

// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[%0], f32>
// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[%1], f32>
// CSE: "test.keep0"
// CSE: "test.keep1"
// PIPE: "test.keep0"
// PIPE: "test.keep1"

// -----

// Dead nat.param should be removed by DCE.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
}

// DCE: ^bb0:
// DCE: }

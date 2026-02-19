// Purpose: nat.param-focused coverage for parse/print+verify, DCE uses-in-types, and CSE non-merge by dim SSA identity.
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

// VERIFY-LABEL: builtin.module {
// VERIFY: [[M:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY: [[N:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// VERIFY: "test.v"() : () -> !dtensor.vector<[[M]], f32>
// VERIFY: "test.mat"() : () -> !dtensor.matrix<[[M]], [[N]], f32>
// VERIFY: "test.t"() : () -> !dtensor.tensor<[[[M]], [[N]]], f32>
// VERIFY: "test.keep_types"
// VERIFY: }

// CANON-LABEL: builtin.module {
// CANON: "test.keep_types"
// CANON: }

// CSE-LABEL: builtin.module {
// CSE: [[M:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CSE: [[N:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CSE: "test.keep_types"
// CSE: }

// DCE-LABEL: builtin.module {
// DCE: [[M:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// DCE: [[N:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// DCE: "test.keep_types"
// DCE: }

// PIPE-LABEL: builtin.module {
// PIPE: [[M:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE: [[N:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE: "test.keep_types"
// PIPE: }

// -----

// nat.param must be preserved when only used via type params.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %t = "dtensor.empty"() : () -> !dtensor.tensor<[%p], f32>
  %u = "test.id"(%t) : (!dtensor.tensor<[%p], f32>) -> !dtensor.tensor<[%p], f32>
  "test.keep_dce_nat_param"(%u) : (!dtensor.tensor<[%p], f32>) -> ()
}

// DCE-LABEL: builtin.module {
// DCE: [[P:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// DCE: "dtensor.empty"() : () -> !dtensor.tensor<[[[P]]], f32>
// DCE: "test.keep_dce_nat_param"
// DCE: }

// CSE-LABEL: builtin.module {
// CSE: [[P:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CSE: "test.keep_dce_nat_param"
// CSE: }

// PIPE-LABEL: builtin.module {
// PIPE: [[P:%[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE: "test.keep_dce_nat_param"
// PIPE: }

// -----

// CSE must not merge nat.param producers (fresh identity).
builtin.module {
  %p0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %p1 = "dtensor.nat.param"() : () -> !dtensor.nat
  "test.keep_params"(%p0, %p1) : (!dtensor.nat, !dtensor.nat) -> ()
}

// CSE-LABEL: builtin.module {
// CSE: "dtensor.nat.param"() : () -> !dtensor.nat
// CSE: "dtensor.nat.param"() : () -> !dtensor.nat
// CSE: "test.keep_params"
// CSE: }

// -----

// Distinct nat.param dims mean result tensor types differ, so CSE must not merge dtensor.empty.
builtin.module {
  %p0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %p1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %e0 = "dtensor.empty"() : () -> !dtensor.tensor<[%p0], f32>
  %e1 = "dtensor.empty"() : () -> !dtensor.tensor<[%p1], f32>
  "test.keep0"(%e0) : (!dtensor.tensor<[%p0], f32>) -> ()
  "test.keep1"(%e1) : (!dtensor.tensor<[%p1], f32>) -> ()
}

// CSE-LABEL: builtin.module {
// CSE: [[P0:%[0-9]+]] = "dtensor.nat.param"()
// CSE: [[P1:%[0-9]+]] = "dtensor.nat.param"()
// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[[[P0]]], f32>
// CSE: "dtensor.empty"() : () -> !dtensor.tensor<[[[P1]]], f32>
// CSE: "test.keep0"
// CSE: "test.keep1"
// CSE: }

// PIPE-LABEL: builtin.module {
// PIPE: "test.keep0"
// PIPE: "test.keep1"
// PIPE: }

// -----

// Dead nat.param should be removed by DCE.
builtin.module {
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
}

// DCE-LABEL: builtin.module {
// DCE: ^bb0:
// DCE-NOT: dtensor.nat.param
// DCE: }

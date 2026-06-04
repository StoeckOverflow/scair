// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce,tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce,tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

// collapse_shape materializes the canonical ordered product dimension.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %q = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %flat = "dtensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%q], f32>
  "test.keep"(%flat) : (!dtensor.tensor<[%q], f32>) -> ()
}

// CANON-LABEL: builtin.module {
// CANON-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[Q:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// CANON-NEXT:   %[[MN:[0-9]+]] = "dtensor.nat.mul"(%[[M]], %[[N]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CANON-NEXT:   %[[FLAT:[0-9]+]] = "dtensor.collapse_shape"(%[[A]]) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> !dtensor.tensor<[%[[MN]]], f32>
// CANON-NEXT:   "test.keep"(%[[FLAT]]) : (!dtensor.tensor<[%[[MN]]], f32>) -> ()
// CANON-NEXT: }

// PIPE-LABEL: builtin.module {
// PIPE-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// PIPE-NEXT:   %[[MN:[0-9]+]] = "dtensor.nat.mul"(%[[M]], %[[N]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// PIPE-NEXT:   %[[FLAT:[0-9]+]] = "dtensor.collapse_shape"(%[[A]]) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> !dtensor.tensor<[%[[MN]]], f32>
// PIPE-NEXT:   "test.keep"(%[[FLAT]]) : (!dtensor.tensor<[%[[MN]]], f32>) -> ()
// PIPE-NEXT: }

// -----

// Multi-group collapse_shape materializes one ordered product per group.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %q0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %q1 = "dtensor.nat.param"() : () -> !dtensor.nat
  %tiled = "test.tiled"() : () -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %untiled = "dtensor.collapse_shape"(%tiled)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !dtensor.tensor<[%q0, %q1], f32>
  "test.keep"(%untiled) : (!dtensor.tensor<[%q0, %q1], f32>) -> ()
}

// CANON-LABEL: builtin.module {
// CANON-NEXT:   %[[MT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[TM:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[NT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[TN:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[Q0:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[Q1:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[TILED:[0-9]+]] = "test.tiled"() : () -> !dtensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>
// CANON-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.mul"(%[[MT]], %[[TM]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CANON-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.mul"(%[[NT]], %[[TN]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CANON-NEXT:   %[[UNTILED:[0-9]+]] = "dtensor.collapse_shape"(%[[TILED]]) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!dtensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>) -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// CANON-NEXT:   "test.keep"(%[[UNTILED]]) : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// CANON-NEXT: }

// PIPE-LABEL: builtin.module {
// PIPE-NEXT:   %[[MT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE-NEXT:   %[[TM:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE-NEXT:   %[[NT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE-NEXT:   %[[TN:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE-NEXT:   %[[TILED:[0-9]+]] = "test.tiled"() : () -> !dtensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>
// PIPE-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.mul"(%[[MT]], %[[TM]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// PIPE-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.mul"(%[[NT]], %[[TN]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// PIPE-NEXT:   %[[UNTILED:[0-9]+]] = "dtensor.collapse_shape"(%[[TILED]]) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!dtensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>) -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// PIPE-NEXT:   "test.keep"(%[[UNTILED]]) : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// PIPE-NEXT: }

// -----

// join_dim materializes exactly the joined pair product.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %q = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %c = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%q, %n], f32>
  "test.keep"(%c) : (!dtensor.tensor<[%q, %n], f32>) -> ()
}

// CANON-LABEL: builtin.module {
// CANON-NEXT:   %[[MT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[TM:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[Q:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// CANON-NEXT:   %[[B:[0-9]+]] = "test.b"() : () -> !dtensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>
// CANON-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.mul"(%[[MT]], %[[TM]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CANON-NEXT:   %[[C:[0-9]+]] = "dtensor.join_dim"(%[[B]]) <{dim = 0 : i32}> : (!dtensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>) -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// CANON-NEXT:   "test.keep"(%[[C]]) : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// CANON-NEXT: }

// PIPE-LABEL: builtin.module {
// PIPE-NEXT:   %[[MT:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE-NEXT:   %[[TM:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE-NEXT:   %[[N:[0-9]+]] = "dtensor.nat.param"() : () -> !dtensor.nat
// PIPE-NEXT:   %[[B:[0-9]+]] = "test.b"() : () -> !dtensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>
// PIPE-NEXT:   %[[M:[0-9]+]] = "dtensor.nat.mul"(%[[MT]], %[[TM]]) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// PIPE-NEXT:   %[[C:[0-9]+]] = "dtensor.join_dim"(%[[B]]) <{dim = 0 : i32}> : (!dtensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>) -> !dtensor.tensor<[%[[M]], %[[N]]], f32>
// PIPE-NEXT:   "test.keep"(%[[C]]) : (!dtensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// PIPE-NEXT: }

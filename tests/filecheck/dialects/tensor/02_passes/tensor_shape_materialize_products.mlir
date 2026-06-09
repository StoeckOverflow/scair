// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce,tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce,tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

// collapse_shape materializes the canonical ordered product dimension.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %q = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %flat = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%q], f32>
  "test.keep"(%flat) : (!d_tensor.tensor<[%q], f32>) -> ()
}

// CANON-LABEL: builtin.module {
// CANON-NEXT:   %[[M:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[N:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[Q:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// CANON-NEXT:   %[[MN:[0-9]+]] = "d_tensor.nat.mul"(%[[M]], %[[N]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CANON-NEXT:   %[[FLAT:[0-9]+]] = "d_tensor.collapse_shape"(%[[A]]) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> !d_tensor.tensor<[%[[MN]]], f32>
// CANON-NEXT:   "test.keep"(%[[FLAT]]) : (!d_tensor.tensor<[%[[MN]]], f32>) -> ()
// CANON-NEXT: }

// PIPE-LABEL: builtin.module {
// PIPE-NEXT:   %[[M:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE-NEXT:   %[[N:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// PIPE-NEXT:   %[[MN:[0-9]+]] = "d_tensor.nat.mul"(%[[M]], %[[N]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// PIPE-NEXT:   %[[FLAT:[0-9]+]] = "d_tensor.collapse_shape"(%[[A]]) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> !d_tensor.tensor<[%[[MN]]], f32>
// PIPE-NEXT:   "test.keep"(%[[FLAT]]) : (!d_tensor.tensor<[%[[MN]]], f32>) -> ()
// PIPE-NEXT: }

// -----

// Multi-group collapse_shape materializes one ordered product per group.
builtin.module {
  %mt = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tm = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %nt = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %q0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %q1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tiled = "test.tiled"() : () -> !d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %untiled = "d_tensor.collapse_shape"(%tiled)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32]]}>
    : (!d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !d_tensor.tensor<[%q0, %q1], f32>
  "test.keep"(%untiled) : (!d_tensor.tensor<[%q0, %q1], f32>) -> ()
}

// CANON-LABEL: builtin.module {
// CANON-NEXT:   %[[MT:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[TM:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[NT:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[TN:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[Q0:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[Q1:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[TILED:[0-9]+]] = "test.tiled"() : () -> !d_tensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>
// CANON-NEXT:   %[[M:[0-9]+]] = "d_tensor.nat.mul"(%[[MT]], %[[TM]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CANON-NEXT:   %[[N:[0-9]+]] = "d_tensor.nat.mul"(%[[NT]], %[[TN]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CANON-NEXT:   %[[UNTILED:[0-9]+]] = "d_tensor.collapse_shape"(%[[TILED]]) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!d_tensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>) -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// CANON-NEXT:   "test.keep"(%[[UNTILED]]) : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// CANON-NEXT: }

// PIPE-LABEL: builtin.module {
// PIPE-NEXT:   %[[MT:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE-NEXT:   %[[TM:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE-NEXT:   %[[NT:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE-NEXT:   %[[TN:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE-NEXT:   %[[TILED:[0-9]+]] = "test.tiled"() : () -> !d_tensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>
// PIPE-NEXT:   %[[M:[0-9]+]] = "d_tensor.nat.mul"(%[[MT]], %[[TM]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// PIPE-NEXT:   %[[N:[0-9]+]] = "d_tensor.nat.mul"(%[[NT]], %[[TN]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// PIPE-NEXT:   %[[UNTILED:[0-9]+]] = "d_tensor.collapse_shape"(%[[TILED]]) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!d_tensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>) -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// PIPE-NEXT:   "test.keep"(%[[UNTILED]]) : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// PIPE-NEXT: }

// -----

// join_dim materializes exactly the joined pair product.
builtin.module {
  %mt = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tm = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %q = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm, %n], f32>
  %c = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm, %n], f32>) -> !d_tensor.tensor<[%q, %n], f32>
  "test.keep"(%c) : (!d_tensor.tensor<[%q, %n], f32>) -> ()
}

// CANON-LABEL: builtin.module {
// CANON-NEXT:   %[[MT:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[TM:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[N:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[Q:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CANON-NEXT:   %[[B:[0-9]+]] = "test.b"() : () -> !d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>
// CANON-NEXT:   %[[M:[0-9]+]] = "d_tensor.nat.mul"(%[[MT]], %[[TM]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CANON-NEXT:   %[[C:[0-9]+]] = "d_tensor.join_dim"(%[[B]]) <{dim = 0 : i32}> : (!d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>) -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// CANON-NEXT:   "test.keep"(%[[C]]) : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// CANON-NEXT: }

// PIPE-LABEL: builtin.module {
// PIPE-NEXT:   %[[MT:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE-NEXT:   %[[TM:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE-NEXT:   %[[N:[0-9]+]] = "d_tensor.nat.param"() : () -> !d_tensor.nat
// PIPE-NEXT:   %[[B:[0-9]+]] = "test.b"() : () -> !d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>
// PIPE-NEXT:   %[[M:[0-9]+]] = "d_tensor.nat.mul"(%[[MT]], %[[TM]]) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// PIPE-NEXT:   %[[C:[0-9]+]] = "d_tensor.join_dim"(%[[B]]) <{dim = 0 : i32}> : (!d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>) -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// PIPE-NEXT:   "test.keep"(%[[C]]) : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> ()
// PIPE-NEXT: }

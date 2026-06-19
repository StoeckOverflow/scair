// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce,tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p tensor-shape-canonicalize,canonicalize,cse,dce,tensor-shape-canonicalize,canonicalize,cse,dce | scair-opt --allow-unregistered-dialect --verify-diagnostics --split-input-file

// collapse_shape materializes its result dim as an index product.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %q = "test.index"() : () -> index
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %flat = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%q], f32>
  "test.keep"(%flat) : (!d_tensor.tensor<[%q], f32>) -> ()
}

// CANON-LABEL: builtin.module {
// CANON-NEXT:   %[[M:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[N:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[Q:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// CANON-NEXT:   %[[MN:[0-9]+]] = "arith.muli"(%[[M]], %[[N]]) {{.*}} : (index, index) -> index
// CANON-NEXT:   %[[FLAT:[0-9]+]] = "d_tensor.collapse_shape"(%[[A]]) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> !d_tensor.tensor<[%[[MN]]], f32>
// CANON-NEXT:   "test.keep"(%[[FLAT]]) : (!d_tensor.tensor<[%[[MN]]], f32>) -> ()
// CANON-NEXT: }

// PIPE-LABEL: builtin.module {
// PIPE-NEXT:   %[[M:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[N:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[Q:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[A:[0-9]+]] = "test.a"() : () -> !d_tensor.tensor<[%[[M]], %[[N]]], f32>
// PIPE-NEXT:   %[[MN:[0-9]+]] = "arith.muli"(%[[M]], %[[N]]) {{.*}} : (index, index) -> index
// PIPE-NEXT:   %[[FLAT:[0-9]+]] = "d_tensor.collapse_shape"(%[[A]]) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%[[M]], %[[N]]], f32>) -> !d_tensor.tensor<[%[[MN]]], f32>
// PIPE-NEXT:   "test.keep"(%[[FLAT]]) : (!d_tensor.tensor<[%[[MN]]], f32>) -> ()
// PIPE-NEXT: }

// -----

// Multi-group collapse_shape materializes one index product per reassociation group.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %nt = "test.index"() : () -> index
  %tn = "test.index"() : () -> index
  %q0 = "test.index"() : () -> index
  %q1 = "test.index"() : () -> index
  %grouped = "test.grouped"() : () -> !d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %collapsed = "d_tensor.collapse_shape"(%grouped)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32]]}>
    : (!d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !d_tensor.tensor<[%q0, %q1], f32>
  "test.keep"(%collapsed) : (!d_tensor.tensor<[%q0, %q1], f32>) -> ()
}

// CANON-LABEL: builtin.module {
// CANON-NEXT:   %[[MT:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[TM:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[NT:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[TN:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[Q0:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[Q1:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[GROUPED:[0-9]+]] = "test.grouped"() : () -> !d_tensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>
// CANON-NEXT:   %[[MTTM:[0-9]+]] = "arith.muli"(%[[MT]], %[[TM]]) {{.*}} : (index, index) -> index
// CANON-NEXT:   %[[NTTN:[0-9]+]] = "arith.muli"(%[[NT]], %[[TN]]) {{.*}} : (index, index) -> index
// CANON-NEXT:   %[[COLLAPSED:[0-9]+]] = "d_tensor.collapse_shape"(%[[GROUPED]]) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!d_tensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>) -> !d_tensor.tensor<[%[[MTTM]], %[[NTTN]]], f32>
// CANON-NEXT:   "test.keep"(%[[COLLAPSED]]) : (!d_tensor.tensor<[%[[MTTM]], %[[NTTN]]], f32>) -> ()
// CANON-NEXT: }

// PIPE-LABEL: builtin.module {
// PIPE-NEXT:   %[[MT:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[TM:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[NT:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[TN:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[Q0:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[Q1:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[GROUPED:[0-9]+]] = "test.grouped"() : () -> !d_tensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>
// PIPE-NEXT:   %[[MTTM:[0-9]+]] = "arith.muli"(%[[MT]], %[[TM]]) {{.*}} : (index, index) -> index
// PIPE-NEXT:   %[[NTTN:[0-9]+]] = "arith.muli"(%[[NT]], %[[TN]]) {{.*}} : (index, index) -> index
// PIPE-NEXT:   %[[COLLAPSED:[0-9]+]] = "d_tensor.collapse_shape"(%[[GROUPED]]) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!d_tensor.tensor<[%[[MT]], %[[TM]], %[[NT]], %[[TN]]], f32>) -> !d_tensor.tensor<[%[[MTTM]], %[[NTTN]]], f32>
// PIPE-NEXT:   "test.keep"(%[[COLLAPSED]]) : (!d_tensor.tensor<[%[[MTTM]], %[[NTTN]]], f32>) -> ()
// PIPE-NEXT: }

// -----

// join_dim materializes its joined dimension as an index product.
builtin.module {
  %mt = "test.index"() : () -> index
  %tm = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %q = "test.index"() : () -> index
  %b = "test.b"() : () -> !d_tensor.tensor<[%mt, %tm, %n], f32>
  %c = "d_tensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!d_tensor.tensor<[%mt, %tm, %n], f32>) -> !d_tensor.tensor<[%q, %n], f32>
  "test.keep"(%c) : (!d_tensor.tensor<[%q, %n], f32>) -> ()
}

// CANON-LABEL: builtin.module {
// CANON-NEXT:   %[[MT:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[TM:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[N:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[Q:[0-9]+]] = "test.index"() : () -> index
// CANON-NEXT:   %[[B:[0-9]+]] = "test.b"() : () -> !d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>
// CANON-NEXT:   %[[MTTM:[0-9]+]] = "arith.muli"(%[[MT]], %[[TM]]) {{.*}} : (index, index) -> index
// CANON-NEXT:   %[[C:[0-9]+]] = "d_tensor.join_dim"(%[[B]]) <{dim = 0 : i32}> : (!d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>) -> !d_tensor.tensor<[%[[MTTM]], %[[N]]], f32>
// CANON-NEXT:   "test.keep"(%[[C]]) : (!d_tensor.tensor<[%[[MTTM]], %[[N]]], f32>) -> ()
// CANON-NEXT: }

// PIPE-LABEL: builtin.module {
// PIPE-NEXT:   %[[MT:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[TM:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[N:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[Q:[0-9]+]] = "test.index"() : () -> index
// PIPE-NEXT:   %[[B:[0-9]+]] = "test.b"() : () -> !d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>
// PIPE-NEXT:   %[[MTTM:[0-9]+]] = "arith.muli"(%[[MT]], %[[TM]]) {{.*}} : (index, index) -> index
// PIPE-NEXT:   %[[C:[0-9]+]] = "d_tensor.join_dim"(%[[B]]) <{dim = 0 : i32}> : (!d_tensor.tensor<[%[[MT]], %[[TM]], %[[N]]], f32>) -> !d_tensor.tensor<[%[[MTTM]], %[[N]]], f32>
// PIPE-NEXT:   "test.keep"(%[[C]]) : (!d_tensor.tensor<[%[[MTTM]], %[[N]]], f32>) -> ()
// PIPE-NEXT: }

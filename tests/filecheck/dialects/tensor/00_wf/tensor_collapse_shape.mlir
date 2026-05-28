// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: 2D to 1D collapse with direct nat.mul-backed result dim.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.mul"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %flat = "dtensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mn], f32>
  "test.keep"(%flat) : (!dtensor.tensor<[%mn], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.a"() : () -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "dtensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: 4D to 2D collapse for exact tiling-shaped tensors.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %n = "dtensor.nat.mul"(%nt, %tn) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %tiled = "test.tiled"() : () -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %untiled = "dtensor.collapse_shape"(%tiled)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32]]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%untiled) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %4 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %5 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %6 = "test.tiled"() : () -> !dtensor.tensor<[%0, %1, %2, %3], f32>
// CHECK-NEXT:   %7 = "dtensor.collapse_shape"(%6) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!dtensor.tensor<[%0, %1, %2, %3], f32>) -> !dtensor.tensor<[%4, %5], f32>
// CHECK-NEXT:   "test.keep"(%7) : (!dtensor.tensor<[%4, %5], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: result dimension may be unrelated before canonicalization.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %other = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %structural = "dtensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%other], f32>
  "test.keep"(%structural) : (!dtensor.tensor<[%other], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.a"() : () -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "dtensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: product order is not checked by the verifier.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %nm = "dtensor.nat.mul"(%n, %m) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %structural = "dtensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%nm], f32>
  "test.keep"(%structural) : (!dtensor.tensor<[%nm], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.mul"(%1, %0) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.a"() : () -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "dtensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: missing product provenance is materialized by tensor-shape-canonicalize.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %structural = "dtensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mn], f32>
  "test.keep"(%structural) : (!dtensor.tensor<[%mn], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.a"() : () -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "dtensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: reassociation must be contiguous and complete over source dims.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %p = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.mul"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n, %p], f32>
  %bad = "dtensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 2 : i32], [1 : i32]]}>
    : (!dtensor.tensor<[%m, %n, %p], f32>) -> !dtensor.tensor<[%mn, %p], f32>
}

// CHECK: dtensor.collapse_shape: reassociation must cover source dims contiguously

// -----

// Invalid: malformed reassociation group.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.mul"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.collapse_shape"(%a)
    <{reassociation = [0 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mn], f32>
}

// CHECK: dtensor.collapse_shape: reassociation group 0 must be an array attribute

// -----

// Invalid: element type must be preserved.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.mul"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mn], i32>
}

// CHECK: dtensor.collapse_shape: expected equal element types

// -----

// Invalid: collapse_shape cannot increase rank.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mn = "dtensor.nat.mul"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%mn], f32>
  %bad = "dtensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32], [1 : i32]]}>
    : (!dtensor.tensor<[%mn], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK: dtensor.collapse_shape: expected result rank <= source rank

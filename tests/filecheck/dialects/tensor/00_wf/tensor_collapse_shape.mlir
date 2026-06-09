// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: 2D to 1D collapse with direct nat.mul-backed result dim.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.mul"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %flat = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%mn], f32>
  "test.keep"(%flat) : (!d_tensor.tensor<[%mn], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %2 = "d_tensor.nat.mul"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CHECK-NEXT:   %3 = "test.a"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: 4D to 2D collapse for exact tiling-shaped tensors.
builtin.module {
  %mt = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tm = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %nt = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %m = "d_tensor.nat.mul"(%mt, %tm) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %n = "d_tensor.nat.mul"(%nt, %tn) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %tiled = "test.tiled"() : () -> !d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %untiled = "d_tensor.collapse_shape"(%tiled)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32]]}>
    : (!d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%untiled) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %3 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %4 = "d_tensor.nat.mul"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CHECK-NEXT:   %5 = "d_tensor.nat.mul"(%2, %3) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CHECK-NEXT:   %6 = "test.tiled"() : () -> !d_tensor.tensor<[%0, %1, %2, %3], f32>
// CHECK-NEXT:   %7 = "d_tensor.collapse_shape"(%6) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1, %2, %3], f32>) -> !d_tensor.tensor<[%4, %5], f32>
// CHECK-NEXT:   "test.keep"(%7) : (!d_tensor.tensor<[%4, %5], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: result dimension may be unrelated before canonicalization.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %other = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %structural = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%other], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%other], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %3 = "test.a"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: product order is not checked by the verifier.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %nm = "d_tensor.nat.mul"(%n, %m) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %structural = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%nm], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%nm], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %2 = "d_tensor.nat.mul"(%1, %0) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CHECK-NEXT:   %3 = "test.a"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: missing product provenance is materialized by tensor-shape-canonicalize.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %structural = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%mn], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%mn], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %1 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %2 = "d_tensor.nat.param"() : () -> !d_tensor.nat
// CHECK-NEXT:   %3 = "test.a"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: reassociation must be contiguous and complete over source dims.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %p = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.mul"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n, %p], f32>
  %bad = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 2 : i32], [1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n, %p], f32>) -> !d_tensor.tensor<[%mn, %p], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation must cover source dims contiguously

// -----

// Invalid: malformed reassociation group.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.mul"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%a)
    <{reassociation = [0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%mn], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation group 0 must be an array attribute

// -----

// Invalid: element type must be preserved.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.mul"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%mn], i32>
}

// CHECK: d_tensor.collapse_shape: expected equal element types

// -----

// Invalid: collapse_shape cannot increase rank.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %mn = "d_tensor.nat.mul"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%mn], f32>
  %bad = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32], [1 : i32]]}>
    : (!d_tensor.tensor<[%mn], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.collapse_shape: expected result rank <= source rank

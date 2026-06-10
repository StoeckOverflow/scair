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

// Valid structural IR: no product/equality witness is present before canonicalization.
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

// Valid structural IR: no required product provenance.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %flat = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %structural = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%flat], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%flat], f32>) -> ()
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

// -----

// Valid: rank-0 collapse_shape identity.
builtin.module {
  %src = "test.src"() : () -> !d_tensor.tensor<[], f32>
  %same = "d_tensor.collapse_shape"(%src)
    <{reassociation = []}>
    : (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
  "test.keep"(%same) : (!d_tensor.tensor<[], f32>) -> ()
}

// CHECK: "d_tensor.collapse_shape"
// CHECK-SAME: <{reassociation = {{\[\]}}}>
// CHECK-SAME: (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>

// -----

// Valid: rank-1 collapse_shape identity.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %same = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32]]}>
    : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], f32>
  "test.keep"(%same) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// CHECK: "d_tensor.collapse_shape"
// CHECK-SAME: (!d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>

// -----

// Valid: rank-preserving collapse_shape identity across multiple dims.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %same = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32], [1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%same) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: "d_tensor.collapse_shape"
// CHECK-SAME: reassociation = {{\[\[0 : i32\], \[1 : i32\]\]}}

// -----

// Valid: static nat.const dimensions still use the SSA-dimension path.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %flat = "d_tensor.nat.const"() <{value = 6 : i32}> : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %collapsed = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%flat], f32>
  "test.keep"(%collapsed) : (!d_tensor.tensor<[%flat], f32>) -> ()
}

// CHECK: "d_tensor.collapse_shape"
// CHECK-SAME: (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>

// -----

// Invalid: reassociation index must be an i32 integer attribute.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i64]]}>
    : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation group 0 index 0 must be an i32 integer attribute

// -----

// Invalid: reassociation group count must equal result rank.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32], [1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.collapse_shape: expected 1 reassociation groups, got 2

// -----

// Invalid: reassociation groups must be non-empty.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[]]}>
    : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation groups must be non-empty

// -----

// Invalid: reassociation indices must be non-negative.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[-1 : i32]]}>
    : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation indices must be non-negative

// -----

// Invalid: reassociation must not miss a source dim.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %flat = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%flat], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation must cover source dims contiguously

// -----

// Invalid: reassociation must not duplicate a source dim.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %flat = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32, 0 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%flat], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation must cover source dims contiguously

// -----

// Invalid: reassociation must not name an extra source dim.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %flat = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32, 1 : i32, 2 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%flat], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation must cover source dims contiguously

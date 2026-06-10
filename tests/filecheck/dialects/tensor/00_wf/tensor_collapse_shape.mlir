// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: 2D to 1D collapse with direct size.mul-backed result dim.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %mn = "d_tensor.size.mul"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %flat = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%mn], f32>
  "test.keep"(%flat) : (!d_tensor.tensor<[%mn], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.mul"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CHECK-NEXT:   %3 = "test.a"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: 4D to 2D collapse for exact tiling-shaped tensors.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %nt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tn = "d_tensor.size.param"() : () -> !d_tensor.size
  %m = "d_tensor.size.mul"(%mt, %tm) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %n = "d_tensor.size.mul"(%nt, %tn) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %tiled = "test.tiled"() : () -> !d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %untiled = "d_tensor.collapse_shape"(%tiled)
    <{reassociation = [[0 : i32, 1 : i32], [2 : i32, 3 : i32]]}>
    : (!d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%untiled) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %3 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %4 = "d_tensor.size.mul"(%0, %1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CHECK-NEXT:   %5 = "d_tensor.size.mul"(%2, %3) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
// CHECK-NEXT:   %6 = "test.tiled"() : () -> !d_tensor.tensor<[%0, %1, %2, %3], f32>
// CHECK-NEXT:   %7 = "d_tensor.collapse_shape"(%6) <{reassociation = {{\[\[0 : i32, 1 : i32\], \[2 : i32, 3 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1, %2, %3], f32>) -> !d_tensor.tensor<[%4, %5], f32>
// CHECK-NEXT:   "test.keep"(%7) : (!d_tensor.tensor<[%4, %5], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: no product/equality witness is present before canonicalization.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %other = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %structural = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%other], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%other], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %3 = "test.a"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: no required product provenance.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %flat = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %structural = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%flat], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%flat], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %3 = "test.a"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid structural IR: missing product provenance is materialized by tensor-shape-canonicalize.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %mn = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %structural = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%mn], f32>
  "test.keep"(%structural) : (!d_tensor.tensor<[%mn], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %1 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %2 = "d_tensor.size.param"() : () -> !d_tensor.size
// CHECK-NEXT:   %3 = "test.a"() : () -> !d_tensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   %4 = "d_tensor.collapse_shape"(%3) <{reassociation = {{\[\[0 : i32, 1 : i32\]\]}}}> : (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%2], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!d_tensor.tensor<[%2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: reassociation must be contiguous and complete over source dims.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %p = "d_tensor.size.param"() : () -> !d_tensor.size
  %mn = "d_tensor.size.mul"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n, %p], f32>
  %bad = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 2 : i32], [1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n, %p], f32>) -> !d_tensor.tensor<[%mn, %p], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation must cover source dims contiguously

// -----

// Invalid: malformed reassociation group.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %mn = "d_tensor.size.mul"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%a)
    <{reassociation = [0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%mn], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation group 0 must be an array attribute

// -----

// Invalid: element type must be preserved.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %mn = "d_tensor.size.mul"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%a)
    <{reassociation = [[0 : i32, 1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%mn], i32>
}

// CHECK: d_tensor.collapse_shape: expected equal element types

// -----

// Invalid: collapse_shape cannot increase rank.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %mn = "d_tensor.size.mul"(%m, %n) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
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
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
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
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %same = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32], [1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%same) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: "d_tensor.collapse_shape"
// CHECK-SAME: reassociation = {{\[\[0 : i32\], \[1 : i32\]\]}}

// -----

// Valid: static size.const dimensions still use the SSA-dimension path.
builtin.module {
  %m = "d_tensor.size.const"() <{value = 2 : i32}> : () -> !d_tensor.size
  %n = "d_tensor.size.const"() <{value = 3 : i32}> : () -> !d_tensor.size
  %flat = "d_tensor.size.const"() <{value = 6 : i32}> : () -> !d_tensor.size
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
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i64]]}>
    : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation group 0 index 0 must be an i32 integer attribute

// -----

// Invalid: reassociation group count must equal result rank.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32], [1 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.collapse_shape: expected 1 reassociation groups, got 2

// -----

// Invalid: reassociation groups must be non-empty.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[]]}>
    : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation groups must be non-empty

// -----

// Invalid: reassociation indices must be non-negative.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[-1 : i32]]}>
    : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation indices must be non-negative

// -----

// Invalid: reassociation must not miss a source dim.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %flat = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%flat], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation must cover source dims contiguously

// -----

// Invalid: reassociation must not duplicate a source dim.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %flat = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32, 0 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%flat], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation must cover source dims contiguously

// -----

// Invalid: reassociation must not name an extra source dim.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %flat = "d_tensor.size.param"() : () -> !d_tensor.size
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.collapse_shape"(%src)
    <{reassociation = [[0 : i32, 1 : i32, 2 : i32]]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%flat], f32>
}

// CHECK: d_tensor.collapse_shape: reassociation must cover source dims contiguously

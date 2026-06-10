// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: identity permutation preserves dimensions by SSA identity.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %out = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32, 1 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
  "test.keep"(%out) : (!d_tensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: "d_tensor.permute_dims"
// CHECK-SAME: permutation = {{\[0 : i32, 1 : i32\]}}
// CHECK-SAME: !d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%0, %1], f32>

// -----

// Valid: 2D swap.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %out = "d_tensor.permute_dims"(%a)
    <{permutation = [1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%n, %m], f32>
  "test.keep"(%out) : (!d_tensor.tensor<[%n, %m], f32>) -> ()
}

// CHECK: "d_tensor.permute_dims"
// CHECK-SAME: permutation = {{\[1 : i32, 0 : i32\]}}
// CHECK-SAME: !d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%1, %0], f32>

// -----

// Valid: rank-0 permutation.
builtin.module {
  %src = "test.src"() : () -> !d_tensor.tensor<[], f32>
  %out = "d_tensor.permute_dims"(%src)
    <{permutation = []}>
    : (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>
  "test.keep"(%out) : (!d_tensor.tensor<[], f32>) -> ()
}

// CHECK: "d_tensor.permute_dims"
// CHECK-SAME: permutation = {{\[\]}}
// CHECK-SAME: (!d_tensor.tensor<[], f32>) -> !d_tensor.tensor<[], f32>

// -----

// Valid: rank-1 identity permutation.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %out = "d_tensor.permute_dims"(%src)
    <{permutation = [0 : i32]}>
    : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], f32>
  "test.keep"(%out) : (!d_tensor.tensor<[%m], f32>) -> ()
}

// CHECK: "d_tensor.permute_dims"
// CHECK-SAME: permutation = {{\[0 : i32\]}}
// CHECK-SAME: (!d_tensor.tensor<[%0], f32>) -> !d_tensor.tensor<[%0], f32>

// -----

// Valid: 3D reverse permutation.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %p = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n, %p], f32>
  %out = "d_tensor.permute_dims"(%src)
    <{permutation = [2 : i32, 1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n, %p], f32>) -> !d_tensor.tensor<[%p, %n, %m], f32>
  "test.keep"(%out) : (!d_tensor.tensor<[%p, %n, %m], f32>) -> ()
}

// CHECK: "d_tensor.permute_dims"
// CHECK-SAME: permutation = {{\[2 : i32, 1 : i32, 0 : i32\]}}
// CHECK-SAME: (!d_tensor.tensor<[%0, %1, %2], f32>) -> !d_tensor.tensor<[%2, %1, %0], f32>

// -----

// Valid: static nat.const dimensions still use the SSA-dimension path.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %out = "d_tensor.permute_dims"(%src)
    <{permutation = [1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%n, %m], f32>
  "test.keep"(%out) : (!d_tensor.tensor<[%n, %m], f32>) -> ()
}

// CHECK: "d_tensor.permute_dims"
// CHECK-SAME: (!d_tensor.tensor<[%0, %1], f32>) -> !d_tensor.tensor<[%1, %0], f32>

// -----

// Valid: 4D tile-major permutation.
builtin.module {
  %mt = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tm = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %nt = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %c = "test.c"() : () -> !d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %out = "d_tensor.permute_dims"(%c)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !d_tensor.tensor<[%mt, %nt, %tm, %tn], f32>
  "test.keep"(%out) : (!d_tensor.tensor<[%mt, %nt, %tm, %tn], f32>) -> ()
}

// CHECK: "d_tensor.permute_dims"
// CHECK-SAME: permutation = {{\[0 : i32, 2 : i32, 1 : i32, 3 : i32\]}}
// CHECK-SAME: !d_tensor.tensor<[%0, %1, %2, %3], f32>) -> !d_tensor.tensor<[%0, %2, %1, %3], f32>

// -----

// Valid: tile-major permutation is its own inverse.
builtin.module {
  %mt = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tm = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %nt = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %tn = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %c = "test.c"() : () -> !d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %tile_major = "d_tensor.permute_dims"(%c)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !d_tensor.tensor<[%mt, %nt, %tm, %tn], f32>
  %round_trip = "d_tensor.permute_dims"(%tile_major)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!d_tensor.tensor<[%mt, %nt, %tm, %tn], f32>) -> !d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>
  "test.keep"(%round_trip) : (!d_tensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> ()
}

// CHECK: "d_tensor.permute_dims"
// CHECK-SAME: !d_tensor.tensor<[%0, %1, %2, %3], f32>) -> !d_tensor.tensor<[%0, %2, %1, %3], f32>
// CHECK: "d_tensor.permute_dims"
// CHECK-SAME: !d_tensor.tensor<[%0, %2, %1, %3], f32>) -> !d_tensor.tensor<[%0, %1, %2, %3], f32>

// -----

// Invalid: wrong result rank.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32, 1 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.permute_dims: expected equal ranks

// -----

// Invalid: wrong permutation length.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32, 1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: expected permutation length 2, got 3

// -----

// Invalid: duplicate permutation entry.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: permutation entries must be unique

// -----

// Invalid: missing permutation entry.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: expected permutation length 2, got 1

// -----

// Invalid: negative permutation entry.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [-1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%n, %m], f32>
}

// CHECK: d_tensor.permute_dims: permutation entries must be non-negative

// -----

// Invalid: out-of-bounds permutation entry.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32, 2 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: permutation entries must be less than rank 2

// -----

// Invalid: output dims do not match the declared permutation.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: expected output dims to match the declared permutation

// -----

// Invalid: element type must be preserved.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %n = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%n, %m], i32>
}

// CHECK: d_tensor.permute_dims: expected equal element types

// -----

// Invalid: permutation entries must be i32 integer attributes.
builtin.module {
  %m = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %src = "test.src"() : () -> !d_tensor.tensor<[%m], f32>
  %bad = "d_tensor.permute_dims"(%src)
    <{permutation = [0 : i64]}>
    : (!d_tensor.tensor<[%m], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.permute_dims: permutation index 0 must be an i32 integer attribute

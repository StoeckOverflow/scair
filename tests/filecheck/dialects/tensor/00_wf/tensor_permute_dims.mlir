// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: identity permutation preserves dimensions by SSA identity.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
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
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
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

// Valid: 4D tile-major permutation.
builtin.module {
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %nt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tn = "d_tensor.size.param"() : () -> !d_tensor.size
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
  %mt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tm = "d_tensor.size.param"() : () -> !d_tensor.size
  %nt = "d_tensor.size.param"() : () -> !d_tensor.size
  %tn = "d_tensor.size.param"() : () -> !d_tensor.size
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
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32, 1 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m], f32>
}

// CHECK: d_tensor.permute_dims: expected equal ranks

// -----

// Invalid: wrong permutation length.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32, 1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: expected permutation length 2, got 3

// -----

// Invalid: duplicate permutation entry.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: permutation entries must be unique

// -----

// Invalid: missing permutation entry.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: expected permutation length 2, got 1

// -----

// Invalid: negative permutation entry.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [-1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%n, %m], f32>
}

// CHECK: d_tensor.permute_dims: permutation entries must be non-negative

// -----

// Invalid: out-of-bounds permutation entry.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [0 : i32, 2 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: permutation entries must be less than rank 2

// -----

// Invalid: output dims do not match the declared permutation.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%m, %n], f32>
}

// CHECK: d_tensor.permute_dims: expected output dims to match the declared permutation

// -----

// Invalid: element type must be preserved.
builtin.module {
  %m = "d_tensor.size.param"() : () -> !d_tensor.size
  %n = "d_tensor.size.param"() : () -> !d_tensor.size
  %a = "test.a"() : () -> !d_tensor.tensor<[%m, %n], f32>
  %bad = "d_tensor.permute_dims"(%a)
    <{permutation = [1 : i32, 0 : i32]}>
    : (!d_tensor.tensor<[%m, %n], f32>) -> !d_tensor.tensor<[%n, %m], i32>
}

// CHECK: d_tensor.permute_dims: expected equal element types

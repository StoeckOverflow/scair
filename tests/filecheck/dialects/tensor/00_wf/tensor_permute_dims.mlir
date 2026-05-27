// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: identity permutation preserves dimensions by SSA identity.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %out = "dtensor.permute_dims"(%a)
    <{permutation = [0 : i32, 1 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%out) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: "dtensor.permute_dims"
// CHECK-SAME: permutation = {{\[0 : i32, 1 : i32\]}}
// CHECK-SAME: !dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%0, %1], f32>

// -----

// Valid: 2D swap.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %out = "dtensor.permute_dims"(%a)
    <{permutation = [1 : i32, 0 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%n, %m], f32>
  "test.keep"(%out) : (!dtensor.tensor<[%n, %m], f32>) -> ()
}

// CHECK: "dtensor.permute_dims"
// CHECK-SAME: permutation = {{\[1 : i32, 0 : i32\]}}
// CHECK-SAME: !dtensor.tensor<[%0, %1], f32>) -> !dtensor.tensor<[%1, %0], f32>

// -----

// Valid: 4D tile-major permutation.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %c = "test.c"() : () -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %out = "dtensor.permute_dims"(%c)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !dtensor.tensor<[%mt, %nt, %tm, %tn], f32>
  "test.keep"(%out) : (!dtensor.tensor<[%mt, %nt, %tm, %tn], f32>) -> ()
}

// CHECK: "dtensor.permute_dims"
// CHECK-SAME: permutation = {{\[0 : i32, 2 : i32, 1 : i32, 3 : i32\]}}
// CHECK-SAME: !dtensor.tensor<[%0, %1, %2, %3], f32>) -> !dtensor.tensor<[%0, %2, %1, %3], f32>

// -----

// Valid: tile-major permutation is its own inverse.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %c = "test.c"() : () -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %tile_major = "dtensor.permute_dims"(%c)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !dtensor.tensor<[%mt, %nt, %tm, %tn], f32>
  %round_trip = "dtensor.permute_dims"(%tile_major)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %nt, %tm, %tn], f32>) -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  "test.keep"(%round_trip) : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> ()
}

// CHECK: "dtensor.permute_dims"
// CHECK-SAME: !dtensor.tensor<[%0, %1, %2, %3], f32>) -> !dtensor.tensor<[%0, %2, %1, %3], f32>
// CHECK: "dtensor.permute_dims"
// CHECK-SAME: !dtensor.tensor<[%0, %2, %1, %3], f32>) -> !dtensor.tensor<[%0, %1, %2, %3], f32>

// -----

// Valid: exact 2D tiling as split, split, permute.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %n = "dtensor.nat.mul"(%nt, %tn) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %c = "dtensor.split_dim"(%b) <{dim = 2 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %d = "dtensor.permute_dims"(%c)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !dtensor.tensor<[%mt, %nt, %tm, %tn], f32>
  "test.keep"(%d) : (!dtensor.tensor<[%mt, %nt, %tm, %tn], f32>) -> ()
}

// CHECK: "dtensor.split_dim"
// CHECK-SAME: !dtensor.tensor<[%4, %5], f32>) -> !dtensor.tensor<[%0, %1, %5], f32>
// CHECK: "dtensor.split_dim"
// CHECK-SAME: !dtensor.tensor<[%0, %1, %5], f32>) -> !dtensor.tensor<[%0, %1, %2, %3], f32>
// CHECK: "dtensor.permute_dims"
// CHECK-SAME: !dtensor.tensor<[%0, %1, %2, %3], f32>) -> !dtensor.tensor<[%0, %2, %1, %3], f32>

// -----

// Invalid: wrong result rank.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.permute_dims"(%a)
    <{permutation = [0 : i32, 1 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m], f32>
}

// CHECK: dtensor.permute_dims: expected equal ranks

// -----

// Invalid: wrong permutation length.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.permute_dims"(%a)
    <{permutation = [0 : i32, 1 : i32, 0 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK: dtensor.permute_dims: expected permutation length 2, got 3

// -----

// Invalid: duplicate permutation entry.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.permute_dims"(%a)
    <{permutation = [0 : i32, 0 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK: dtensor.permute_dims: permutation entries must be unique

// -----

// Invalid: missing permutation entry.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.permute_dims"(%a)
    <{permutation = [0 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK: dtensor.permute_dims: expected permutation length 2, got 1

// -----

// Invalid: negative permutation entry.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.permute_dims"(%a)
    <{permutation = [-1 : i32, 0 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%n, %m], f32>
}

// CHECK: dtensor.permute_dims: permutation entries must be non-negative

// -----

// Invalid: out-of-bounds permutation entry.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.permute_dims"(%a)
    <{permutation = [0 : i32, 2 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK: dtensor.permute_dims: permutation entries must be less than rank 2

// -----

// Invalid: output dims do not match the declared permutation.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.permute_dims"(%a)
    <{permutation = [1 : i32, 0 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
}

// CHECK: dtensor.permute_dims: expected output dims to match the declared permutation

// -----

// Invalid: element type must be preserved.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.permute_dims"(%a)
    <{permutation = [1 : i32, 0 : i32]}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%n, %m], i32>
}

// CHECK: dtensor.permute_dims: expected equal element types

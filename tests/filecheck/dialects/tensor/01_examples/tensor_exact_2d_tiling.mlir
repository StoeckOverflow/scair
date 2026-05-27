// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// This file is the focused exact-2D-tiling demo for value-dependent dtensor
// shapes. Exact tiling is represented as typed reshape: split the m axis,
// split the n axis, then permute to tile-major order. The verifier checks
// ordered product provenance and SSA-identity dimension movement. It does not
// solve arithmetic, choose tile sizes, or prove loop dependence legality.

// Valid: exact 2D tiling as split_dim, split_dim, permute_dims.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %n = "dtensor.nat.mul"(%nt, %tn) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.input"() : () -> !dtensor.tensor<[%m, %n], f32>

  %split_m = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %split_n = "dtensor.split_dim"(%split_m) <{dim = 2 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %tile_major = "dtensor.permute_dims"(%split_n)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>)
   -> !dtensor.tensor<[%mt, %nt, %tm, %tn], f32>

  "test.keep"(%tile_major) : (!dtensor.tensor<[%mt, %nt, %tm, %tn], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK:   %3 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK:   %4 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK:   %5 = "dtensor.nat.mul"(%2, %3) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK:   %6 = "test.input"() : () -> !dtensor.tensor<[%4, %5], f32>
// CHECK:   %7 = "dtensor.split_dim"(%6) <{dim = 0 : i32}> : (!dtensor.tensor<[%4, %5], f32>) -> !dtensor.tensor<[%0, %1, %5], f32>
// CHECK:   %8 = "dtensor.split_dim"(%7) <{dim = 2 : i32}> : (!dtensor.tensor<[%0, %1, %5], f32>) -> !dtensor.tensor<[%0, %1, %2, %3], f32>
// CHECK:   %9 = "dtensor.permute_dims"(%8) <{permutation = {{\[0 : i32, 2 : i32, 1 : i32, 3 : i32\]}}}> : (!dtensor.tensor<[%0, %1, %2, %3], f32>) -> !dtensor.tensor<[%0, %2, %1, %3], f32>
// CHECK:   "test.keep"(%9) : (!dtensor.tensor<[%0, %2, %1, %3], f32>) -> ()
// CHECK: }

// -----

// Invalid: first split uses dimensions not justified by product provenance.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.input"() : () -> !dtensor.tensor<[%m, %n], f32>
  %bad = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %n], f32>
}

// CHECK: dtensor.split_dim: expected input dim 0 to equal ordered product of result dims [0, 1]

// -----

// Invalid: second split uses dimensions not justified by product provenance.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.input"() : () -> !dtensor.tensor<[%m, %n], f32>
  %split_m = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %bad = "dtensor.split_dim"(%split_m) <{dim = 2 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
}

// CHECK: dtensor.split_dim: expected input dim 2 to equal ordered product of result dims [2, 3]

// -----

// Invalid: ordered product equality rejects wrong factor order.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %n = "dtensor.nat.mul"(%tn, %nt) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.input"() : () -> !dtensor.tensor<[%m, %n], f32>
  %split_m = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %bad = "dtensor.split_dim"(%split_m) <{dim = 2 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
}

// CHECK: dtensor.split_dim: expected input dim 2 to equal ordered product of result dims [2, 3]

// -----

// Invalid: splitting the wrong second axis is rejected by product provenance.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %n = "dtensor.nat.mul"(%nt, %tn) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.input"() : () -> !dtensor.tensor<[%m, %n], f32>
  %split_m = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %bad = "dtensor.split_dim"(%split_m) <{dim = 1 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%mt, %nt, %tn, %n], f32>
}

// CHECK: dtensor.split_dim: expected input dim 1 to equal ordered product of result dims [1, 2]

// -----

// Invalid: wrong permutation cannot claim tile-major output.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %c = "test.split"() : () -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %bad = "dtensor.permute_dims"(%c)
    <{permutation = [0 : i32, 1 : i32, 2 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !dtensor.tensor<[%mt, %nt, %tm, %tn], f32>
}

// CHECK: dtensor.permute_dims: expected output dims to match the declared permutation

// -----

// Invalid: final result type does not match the declared tile-major permutation.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %c = "test.split"() : () -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %bad = "dtensor.permute_dims"(%c)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
}

// CHECK: dtensor.permute_dims: expected output dims to match the declared permutation

// -----

// Invalid: a shape-correct but semantically different axis order is rejected
// when the op declares a different permutation.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %c = "test.split"() : () -> !dtensor.tensor<[%mt, %tm, %nt, %tn], f32>
  %bad = "dtensor.permute_dims"(%c)
    <{permutation = [0 : i32, 2 : i32, 1 : i32, 3 : i32]}>
    : (!dtensor.tensor<[%mt, %tm, %nt, %tn], f32>) -> !dtensor.tensor<[%nt, %mt, %tm, %tn], f32>
}

// CHECK: dtensor.permute_dims: expected output dims to match the declared permutation

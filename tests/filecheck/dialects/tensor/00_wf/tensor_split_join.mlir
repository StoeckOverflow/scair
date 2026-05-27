// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: 1D exact split.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %b = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%mt, %tm], f32>
  "test.keep"(%b) : (!dtensor.tensor<[%mt, %tm], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %3 = "test.a"() : () -> !dtensor.tensor<[%2], f32>
// CHECK-NEXT:   %4 = "dtensor.split_dim"(%3) <{dim = 0 : i32}> : (!dtensor.tensor<[%2], f32>) -> !dtensor.tensor<[%0, %1], f32>
// CHECK-NEXT:   "test.keep"(%4) : (!dtensor.tensor<[%0, %1], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: 2D split of the first dimension.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %n], f32>
  "test.keep"(%b) : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %4 = "test.a"() : () -> !dtensor.tensor<[%3, %2], f32>
// CHECK-NEXT:   %5 = "dtensor.split_dim"(%4) <{dim = 0 : i32}> : (!dtensor.tensor<[%3, %2], f32>) -> !dtensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   "test.keep"(%5) : (!dtensor.tensor<[%0, %1, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: 2D split of the second dimension.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %nt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tn = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.mul"(%nt, %tn) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "dtensor.split_dim"(%a) <{dim = 1 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%m, %nt, %tn], f32>
  "test.keep"(%b) : (!dtensor.tensor<[%m, %nt, %tn], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.mul"(%1, %2) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %4 = "test.a"() : () -> !dtensor.tensor<[%0, %3], f32>
// CHECK-NEXT:   %5 = "dtensor.split_dim"(%4) <{dim = 1 : i32}> : (!dtensor.tensor<[%0, %3], f32>) -> !dtensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   "test.keep"(%5) : (!dtensor.tensor<[%0, %1, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: join inverse for a split-shaped tensor.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %c = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%c) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %4 = "test.b"() : () -> !dtensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   %5 = "dtensor.join_dim"(%4) <{dim = 0 : i32}> : (!dtensor.tensor<[%0, %1, %2], f32>) -> !dtensor.tensor<[%3, %2], f32>
// CHECK-NEXT:   "test.keep"(%5) : (!dtensor.tensor<[%3, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Valid: split followed by join round trip.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %n = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m, %n], f32>
  %b = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m, %n], f32>) -> !dtensor.tensor<[%mt, %tm, %n], f32>
  %c = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm, %n], f32>) -> !dtensor.tensor<[%m, %n], f32>
  "test.keep"(%c) : (!dtensor.tensor<[%m, %n], f32>) -> ()
}

// CHECK: builtin.module {
// CHECK-NEXT:   %0 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %1 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %2 = "dtensor.nat.param"() : () -> !dtensor.nat
// CHECK-NEXT:   %3 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CHECK-NEXT:   %4 = "test.a"() : () -> !dtensor.tensor<[%3, %2], f32>
// CHECK-NEXT:   %5 = "dtensor.split_dim"(%4) <{dim = 0 : i32}> : (!dtensor.tensor<[%3, %2], f32>) -> !dtensor.tensor<[%0, %1, %2], f32>
// CHECK-NEXT:   %6 = "dtensor.join_dim"(%5) <{dim = 0 : i32}> : (!dtensor.tensor<[%0, %1, %2], f32>) -> !dtensor.tensor<[%3, %2], f32>
// CHECK-NEXT:   "test.keep"(%6) : (!dtensor.tensor<[%3, %2], f32>) -> ()
// CHECK-NEXT: }

// -----

// Invalid: split with unrelated result dimensions.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %other = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %bad = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%mt, %other], f32>
}

// CHECK: dtensor.split_dim: expected input dim 0 to equal ordered product of result dims [0, 1]

// -----

// Invalid: split with missing product provenance.
builtin.module {
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %bad = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%mt, %tm], f32>
}

// CHECK: dtensor.split_dim: expected input dim 0 to equal ordered product of result dims [0, 1]

// -----

// Invalid: split rejects wrong product order.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%tm, %mt) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %bad = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%mt, %tm], f32>
}

// CHECK: dtensor.split_dim: expected input dim 0 to equal ordered product of result dims [0, 1]

// -----

// Invalid: split dim out of bounds.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %bad = "dtensor.split_dim"(%a) <{dim = 1 : i32}>
    : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%mt, %tm], f32>
}

// CHECK: dtensor.split_dim: dim 1 out of bounds for rank 1

// -----

// Invalid: split wrong result rank.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %bad = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%mt, %tm, %m], f32>
}

// CHECK: dtensor.split_dim: expected result rank = input rank + 1

// -----

// Invalid: join with unrelated result dimension.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %other = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %bad = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%other], f32>
}

// CHECK: dtensor.join_dim: expected result dim 0 to equal ordered product of input dims [0, 1]

// -----

// Invalid: join with missing product provenance.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.param"() : () -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %bad = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%m], f32>
}

// CHECK: dtensor.join_dim: expected result dim 0 to equal ordered product of input dims [0, 1]

// -----

// Invalid: join dim out of bounds.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %bad = "dtensor.join_dim"(%b) <{dim = 1 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%m], f32>
}

// CHECK: dtensor.join_dim: dim 1 out of bounds for rank 2

// -----

// Invalid: join wrong result rank.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %bad = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%m, %mt], f32>
}

// CHECK: dtensor.join_dim: expected result rank = input rank - 1

// -----

// Invalid: element type mismatch is rejected for split.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %a = "test.a"() : () -> !dtensor.tensor<[%m], f32>
  %bad = "dtensor.split_dim"(%a) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%m], f32>) -> !dtensor.tensor<[%mt, %tm], i32>
}

// CHECK: dtensor.split_dim: expected equal element types

// -----

// Invalid: element type mismatch is rejected for join.
builtin.module {
  %mt = "dtensor.nat.param"() : () -> !dtensor.nat
  %tm = "dtensor.nat.param"() : () -> !dtensor.nat
  %m = "dtensor.nat.mul"(%mt, %tm) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %b = "test.b"() : () -> !dtensor.tensor<[%mt, %tm], f32>
  %bad = "dtensor.join_dim"(%b) <{dim = 0 : i32}>
    : (!dtensor.tensor<[%mt, %tm], f32>) -> !dtensor.tensor<[%m], i32>
}

// CHECK: dtensor.join_dim: expected equal element types

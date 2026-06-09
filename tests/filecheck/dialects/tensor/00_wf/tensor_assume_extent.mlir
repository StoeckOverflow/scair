// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefixes=VERIFY,DIAG

// Valid: assume_extent is a resultless verification marker on index values.
builtin.module {
  %n = "test.index"() : () -> index
  "d_tensor.assume_extent"(%n) : (index) -> ()
  %t = "test.t"() : () -> !d_tensor.tensor<[%n], f32>
  "test.keep"(%t) : (!d_tensor.tensor<[%n], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.index"() : () -> index
// VERIFY:   "d_tensor.assume_extent"(%0) : (index) -> ()
// VERIFY:   %1 = "test.t"() : () -> !d_tensor.tensor<[%0], f32>
// VERIFY:   "test.keep"(%1) : (!d_tensor.tensor<[%0], f32>) -> ()
// VERIFY: }

// -----

// Valid: dimensions do not require a preceding assume_extent marker.
builtin.module {
  %m = "test.index"() : () -> index
  %n = "test.index"() : () -> index
  %mn = "arith.muli"(%m, %n) : (index, index) -> index
  %t = "test.t"() : () -> !d_tensor.tensor<[%mn], f32>
  "test.keep"(%t) : (!d_tensor.tensor<[%mn], f32>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.index"() : () -> index
// VERIFY:   %1 = "test.index"() : () -> index
// VERIFY:   %2 = "arith.muli"(%0, %1) {{.*}} : (index, index) -> index
// VERIFY:   %3 = "test.t"() : () -> !d_tensor.tensor<[%2], f32>
// VERIFY:   "test.keep"(%3) : (!d_tensor.tensor<[%2], f32>) -> ()
// VERIFY: }

// -----

// Invalid: assume_extent only accepts builtin index operands.
builtin.module {
  %bad = "arith.constant"() <{value = 7 : i32}> : () -> i32
  // expected-error @below {{d_tensor.assume_extent: expected index operand, got i32}}
  "d_tensor.assume_extent"(%bad) : (i32) -> ()
}

// DIAG: d_tensor.assume_extent: expected index operand, got i32

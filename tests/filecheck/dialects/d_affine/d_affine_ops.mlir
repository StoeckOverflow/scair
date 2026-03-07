// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY

builtin.module {
  %lb = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %ub = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %st = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  d_affine.for %iv = %lb to %ub step %st {
    %m = d_affine.min %iv, %ub : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    "test.keep"(%m) : (!dtensor.nat) -> ()
    d_affine.yield
  }
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
// VERIFY:   %1 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
// VERIFY:   %2 = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
// VERIFY:   d_affine.for %3 = %0 to %1 step %2 {
// VERIFY:     %4 = d_affine.min %3, %1 : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// VERIFY:     "test.keep"(%4) : (!dtensor.nat) -> ()
// VERIFY:     d_affine.yield
// VERIFY:   }
// VERIFY: }

// -----

builtin.module {
  d_affine.yield
}

// VERIFY: d_affine.yield: expected parent op d_affine.for

// -----

builtin.module {
  %lb = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %ub = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %st = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  d_affine.for %iv = %lb to %ub step %st {
    d_affine.yield
  }
}

// VERIFY: d_affine.for: expected positive step, got 0

// -----

builtin.module {
  %lb = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %ub = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %st = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  d_affine.for %iv = %lb to %ub step %st {
    "test.no_terminator"() : () -> ()
  }
}

// VERIFY: d_affine.for: expected terminator d_affine.yield

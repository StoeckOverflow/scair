// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY

builtin.module {
  %lb_nat = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %ub_nat = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %lb = "dtensor.shape.to_index"(%lb_nat) : (!dtensor.nat) -> index
  %ub = "dtensor.shape.to_index"(%ub_nat) : (!dtensor.nat) -> index
  d_affine.for %iv = %lb to %ub step 1 : i32 {
    %m = d_affine.min %iv, %ub : (index, index) -> index
    "test.keep"(%m) : (index) -> ()
    d_affine.yield
  }
}

// VERIFY: d_affine.for %{{.*}} = %{{.*}} to %{{.*}} step 1 : i32 {
// VERIFY: %{{.*}} = d_affine.min %{{.*}}, %{{.*}} : (index, index) -> index
// VERIFY: d_affine.yield

// -----

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %r = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%i, %i) : (index, index) -> index
  "test.keep"(%r) : (index) -> ()
}

// VERIFY: #map = affine_map<(d0)[s0] -> (d0 + s0)>
// VERIFY: d_affine.apply #map

// -----

builtin.module {
  d_affine.yield
}

// VERIFY: d_affine.yield: expected parent op d_affine.for

// -----

builtin.module {
  %lb_nat = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %ub_nat = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %lb = "dtensor.shape.to_index"(%lb_nat) : (!dtensor.nat) -> index
  %ub = "dtensor.shape.to_index"(%ub_nat) : (!dtensor.nat) -> index
  d_affine.for %iv = %lb to %ub step 0 : i32 {
    d_affine.yield
  }
}

// VERIFY: d_affine.for: expected positive step, got 0

// -----

builtin.module {
  %lb_nat = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %ub_nat = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
  %lb = "dtensor.shape.to_index"(%lb_nat) : (!dtensor.nat) -> index
  %ub = "dtensor.shape.to_index"(%ub_nat) : (!dtensor.nat) -> index
  d_affine.for %iv = %lb to %ub step 1 : i32 {
    "test.no_terminator"() : () -> ()
  }
}

// VERIFY: d_affine.for: expected terminator d_affine.yield

// -----

builtin.module {
  %i = "arith.constant"() <{value = 7 : index}> : () -> index
  %r = d_affine.apply affine_map<(d0) -> (d0, d0 + 1)>(%i) : (index) -> index
  "test.keep"(%r) : (index) -> ()
}

// VERIFY: d_affine.apply: only single-result affine maps are supported, got 2 results

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | filecheck %s -DFILE=%s --check-prefix=DCE

// CSE + deep RAUW: merged dim value must rewrite type-embedded use.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %s0 = "dtensor.nat.add"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s1 = "dtensor.nat.add"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.use"() : () -> !dtensor.tensor<[%s1], f32>
}

// CSE-LABEL: builtin.module {
// CSE: [[S:%[0-9]+]] = "dtensor.nat.add"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CSE-NOT: "dtensor.nat.add"
// CSE: "test.use"() : () -> !dtensor.tensor<[[[S]]], f32>
// CSE: }

// -----

// DCE removes truly dead nat ops while preserving used dims.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %dead = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.keep"() : () -> !dtensor.tensor<[%m], f32>
}

// DCE-LABEL: builtin.module {
// DCE: "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// DCE: "test.keep"() : () -> !dtensor.tensor<[%0], f32>
// DCE-NOT: "dtensor.nat.add"
// DCE: }

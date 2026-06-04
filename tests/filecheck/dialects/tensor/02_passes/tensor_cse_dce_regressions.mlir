// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics

// CSE + deep RAUW: merged dim value must rewrite type-embedded use.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
  %n = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
  %s0 = "dtensor.nat.add"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s1 = "dtensor.nat.add"(%m, %n) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.use"() : () -> !dtensor.tensor<[%s1], f32>
}

// CSE: builtin.module {
// CSE:   %0 = "dtensor.nat.const"() <{value = 2 : i32}> : () -> !dtensor.nat
// CSE:   %1 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
// CSE:   %2 = "dtensor.nat.add"(%0, %1) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
// CSE:   %3 = "test.use"() : () -> !dtensor.tensor<[%2], f32>
// CSE: }

// -----

// DCE removes truly dead nat ops while preserving used dims.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %dead = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u = "test.keep"() : () -> !dtensor.tensor<[%m], f32>
}

// DCE: builtin.module {
// DCE:   %0 = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// DCE:   %1 = "test.keep"() : () -> !dtensor.tensor<[%0], f32>
// DCE: }

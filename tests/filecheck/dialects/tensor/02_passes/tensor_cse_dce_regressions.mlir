// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | filecheck %s -DFILE=%s --check-prefix=DCE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics

// CSE + deep RAUW: merged dim value must rewrite type-embedded use.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
  %n = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
  %s0 = "d_tensor.nat.add"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %s1 = "d_tensor.nat.add"(%m, %n) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %u = "test.use"() : () -> !d_tensor.tensor<[%s1], f32>
}

// CSE: builtin.module {
// CSE:   %0 = "d_tensor.nat.const"() <{value = 2 : i32}> : () -> !d_tensor.nat
// CSE:   %1 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
// CSE:   %2 = "d_tensor.nat.add"(%0, %1) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
// CSE:   %3 = "test.use"() : () -> !d_tensor.tensor<[%2], f32>
// CSE: }

// -----

// DCE removes truly dead nat ops while preserving used dims.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %z = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %dead = "d_tensor.nat.add"(%m, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %u = "test.keep"() : () -> !d_tensor.tensor<[%m], f32>
}

// DCE: builtin.module {
// DCE:   %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// DCE:   %1 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// DCE: }

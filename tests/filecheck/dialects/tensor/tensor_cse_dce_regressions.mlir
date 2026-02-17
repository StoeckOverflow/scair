// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p cse | filecheck %s -DFILE=%s --check-prefix=CSE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p dce | filecheck %s -DFILE=%s --check-prefix=DCE

// CSE must not merge ops whose result types differ by dim SSA identity.
builtin.module {
  %m0 = "test.nat0"() : () -> !tensor.nat
  %m1 = "test.nat1"() : () -> !tensor.nat
  %a = "tensor.empty"() : () -> !tensor.tensor<[%m0], f32>
  %b = "tensor.empty"() : () -> !tensor.tensor<[%m1], f32>
}

// CSE-LABEL: builtin.module {
// CSE: "tensor.empty"() : () -> !tensor.tensor<[%0], f32>
// CSE: "tensor.empty"() : () -> !tensor.tensor<[%1], f32>
// CSE: }

// -----

// CSE + deep RAUW: merged dim value must rewrite type-embedded use.
builtin.module {
  %m = "tensor.nat.const"() <{value = 2 : i32}> : () -> !tensor.nat
  %n = "tensor.nat.const"() <{value = 3 : i32}> : () -> !tensor.nat
  %s0 = "tensor.nat.add"(%m, %n) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %s1 = "tensor.nat.add"(%m, %n) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %u = "test.use"() : () -> !tensor.tensor<[%s1], f32>
}

// CSE-LABEL: builtin.module {
// CSE: [[S:%[0-9]+]] = "tensor.nat.add"(%0, %1) : (!tensor.nat, !tensor.nat) -> !tensor.nat
// CSE-NOT: "tensor.nat.add"
// CSE: "test.use"() : () -> !tensor.tensor<[[[S]]], f32>
// CSE: }

// -----

// DCE keeps dims used only from type parameters.
builtin.module {
  %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %u = "test.keep"() : () -> !tensor.tensor<[%m], f32>
}

// DCE-LABEL: builtin.module {
// DCE: "test.nat0"() : () -> !tensor.nat
// DCE: "test.nat1"() : () -> !tensor.nat
// DCE: }

// DCE-LABEL: builtin.module {
// DCE: "tensor.nat.add"
// DCE: "test.use"() : () -> !tensor.tensor<[%2], f32>
// DCE: }

// DCE-LABEL: builtin.module {
// DCE: "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
// DCE: "test.keep"() : () -> !tensor.tensor<[%0], f32>
// DCE: }

// -----

// DCE removes truly dead nat ops while preserving used dims.
builtin.module {
  %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %z = "tensor.nat.const"() <{value = 0 : i32}> : () -> !tensor.nat
  %dead = "tensor.nat.add"(%m, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %u = "test.keep"() : () -> !tensor.tensor<[%m], f32>
}

// DCE-LABEL: builtin.module {
// DCE: "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
// DCE: "test.keep"() : () -> !tensor.tensor<[%0], f32>
// DCE-NOT: "tensor.nat.add"
// DCE: }

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

// End-to-end: shape-canon + canonicalize + cse + dce preserves WF and reduces nat ops.
builtin.module {
  %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
  %z = "dtensor.nat.const"() <{value = 0 : i32}> : () -> !dtensor.nat
  %o = "dtensor.nat.const"() <{value = 1 : i32}> : () -> !dtensor.nat
  %s0 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s1 = "dtensor.nat.mul"(%s0, %o) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %s2 = "dtensor.nat.add"(%m, %z) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
  %u0 = "test.keep"() : () -> !dtensor.tensor<[%s1], f32>
  %u1 = "test.keep"() : () -> !dtensor.tensor<[%s2], f32>
}

// PIPE-LABEL: builtin.module {
// PIPE: [[M:%[0-9]+]] = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
// PIPE-NOT: "dtensor.nat.add"
// PIPE-NOT: "dtensor.nat.mul"
// PIPE: "test.keep"() : () -> !dtensor.tensor<[[[M]]], f32>
// PIPE: "test.keep"() : () -> !dtensor.tensor<[[[M]]], f32>
// PIPE: }

// -----

// Negative pipeline: invalid dominance-in-types should still diagnose.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "dtensor.nat.const"() <{value = 4 : i32}> : () -> !dtensor.nat
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %u = "test.use"() : () -> !dtensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// PIPE: ssa-dominance: value Value
// PIPE: does not dominate its use in op `test.use`

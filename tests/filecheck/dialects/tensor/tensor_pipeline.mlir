// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

// End-to-end: shape-canon + canonicalize + cse + dce preserves WF and reduces nat ops.
builtin.module {
  %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
  %z = "tensor.nat.const"() <{value = 0 : i32}> : () -> !tensor.nat
  %o = "tensor.nat.const"() <{value = 1 : i32}> : () -> !tensor.nat
  %s0 = "tensor.nat.add"(%m, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %s1 = "tensor.nat.mul"(%s0, %o) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %s2 = "tensor.nat.add"(%m, %z) : (!tensor.nat, !tensor.nat) -> !tensor.nat
  %u0 = "test.keep"() : () -> !tensor.tensor<[%s1], f32>
  %u1 = "test.keep"() : () -> !tensor.tensor<[%s2], f32>
}

// PIPE-LABEL: builtin.module {
// PIPE: [[M:%[0-9]+]] = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
// PIPE-NOT: "tensor.nat.add"
// PIPE-NOT: "tensor.nat.mul"
// PIPE: "test.keep"() : () -> !tensor.tensor<[[[M]]], f32>
// PIPE: "test.keep"() : () -> !tensor.tensor<[[[M]]], f32>
// PIPE: }

// -----

// Negative pipeline: invalid dominance-in-types should still diagnose.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "tensor.nat.const"() <{value = 4 : i32}> : () -> !tensor.nat
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %u = "test.use"() : () -> !tensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// PIPE: ssa-dominance: value Value
// PIPE: does not dominate its use in op `test.use`

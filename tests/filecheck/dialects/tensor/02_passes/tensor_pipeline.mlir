// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

// End-to-end: shape-canon + canonicalize + cse + dce preserves WF and reduces nat ops.
builtin.module {
  %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
  %z = "d_tensor.nat.const"() <{value = 0 : i32}> : () -> !d_tensor.nat
  %o = "d_tensor.nat.const"() <{value = 1 : i32}> : () -> !d_tensor.nat
  %s0 = "d_tensor.nat.add"(%m, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %s1 = "d_tensor.nat.mul"(%s0, %o) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %s2 = "d_tensor.nat.add"(%m, %z) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
  %u0 = "test.keep"() : () -> !d_tensor.tensor<[%s1], f32>
  %u1 = "test.keep"() : () -> !d_tensor.tensor<[%s2], f32>
}

// PIPE: builtin.module {
// PIPE:   %0 = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
// PIPE:   %1 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE:   %2 = "test.keep"() : () -> !d_tensor.tensor<[%0], f32>
// PIPE: }

// -----

// Negative pipeline: invalid dominance-in-types should still diagnose.
builtin.module {
  "test.region"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %m = "d_tensor.nat.const"() <{value = 4 : i32}> : () -> !d_tensor.nat
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %u = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// PIPE: // -----
// PIPE: ssa-dominance: value Value(!d_tensor.nat) does not dominate its use in op `test.use`

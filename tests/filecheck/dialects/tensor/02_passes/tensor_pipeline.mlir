// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics -p tensor-shape-canonicalize,canonicalize,cse,dce | filecheck %s -DFILE=%s --check-prefix=PIPE

// End-to-end: shape-canon + canonicalize + cse + dce preserves WF and reduces nat ops.
builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %o = "arith.constant"() <{value = 1 : index}> : () -> index
  %s0 = "arith.addi"(%m, %z) : (index, index) -> index
  %s1 = "arith.muli"(%s0, %o) : (index, index) -> index
  %s2 = "arith.addi"(%m, %z) : (index, index) -> index
  %u0 = "test.keep"() : () -> !d_tensor.tensor<[%s1], f32>
  %u1 = "test.keep"() : () -> !d_tensor.tensor<[%s2], f32>
}

// PIPE: builtin.module {
// PIPE:   %0 = "arith.constant"() <{value = 4 : index}> : () -> index
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
    %m = "arith.constant"() <{value = 4 : index}> : () -> index
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %u = "test.use"() : () -> !d_tensor.tensor<[%m], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// PIPE: // -----
// PIPE: ssa-dominance: value Value(index) does not dominate its use in op `test.use`

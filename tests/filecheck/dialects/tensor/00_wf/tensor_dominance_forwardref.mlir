// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s --check-prefix=PARSE

// Dominance-in-types negative: dim does not dominate type use.
builtin.module {
  "test.outer"() ({
  ^bb0:
    %c = "arith.constant"() <{value = true}> : () -> i1
    "test.cond_br"(%c) [^bb1, ^bb2] : (i1) -> ()
  ^bb1:
    %n = "test.index"() : () -> index
    "test.br"() [^bb2] : () -> ()
  ^bb2:
    %t = "test.bad"() : () -> !d_tensor.tensor<[%n], f32>
    "test.ret"() : () -> ()
  }) : () -> ()
}

// VERIFY: ssa-dominance: value Value(index) does not dominate its use in op `test.bad`

// -----

// Forward-reference parsing negative: %n used in tensor type before definition.
builtin.module {
  %t = "test.bad"() : () -> !d_tensor.vector<%n, f32>
  %n = "test.index"() : () -> index
}

// PARSE: ssa-dominance: value Value(index) does not dominate its use in op `test.bad`

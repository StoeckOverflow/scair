// Purpose: Stepwise pipeline checks for the two TLam polymorphism smoke examples.
// Invariants covered: Explicit IR after beta-reduce, monomorphize, erase, and lower.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=BETA
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam,monomorphize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=MONO
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam,monomorphize --verify-diagnostics | scair-opt --allow-unregistered-dialect --split-input-file --verify-diagnostics
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam,monomorphize,dce,erase-tlam --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=ERASE
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam,monomorphize,dce,erase-tlam,lower-tlam-to-func --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=LOWER

// Example 1
builtin.module {
  %dead = "tlam.tlambda"() ({
  ^bb0(%T_dead: !tlam.type):
    %dead_i64 = "builtin.unrealized_conversion_cast"() : () -> i64
    "tlam.treturn"(%dead_i64) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "tlam.vlambda"() ({
    ^bb0(%x: !value<%T>):
      "tlam.vreturn"(%x) : (!value<%T>) -> ()
    }) : () -> !tlam.fun<!value<%T>, !value<%T>>
    "tlam.treturn"(%v)
      : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %f_i64 = "tlam.tapply"(%F) <{tyArg = i64}>
    : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
      -> !tlam.fun<i64, i64>

  %c = "arith.constant"() <{value = 7 : i64}> : () -> i64
  %r = "tlam.vapply"(%f_i64, %c) : (!tlam.fun<i64, i64>, i64) -> i64
  "test.use"(%r) : (i64) -> ()

  %id_inline = "tlam.vlambda"() ({
  ^bb0(%x: i64):
    "tlam.vreturn"(%x) : (i64) -> ()
  }) : () -> !tlam.fun<i64, i64>
  %beta_c = "arith.constant"() <{value = 11 : i64}> : () -> i64
  %beta_r = "tlam.vapply"(%id_inline, %beta_c) : (!tlam.fun<i64, i64>, i64) -> i64
  "test.use"(%beta_r) : (i64) -> ()
}

// BETA: builtin.module {
// BETA:   %0 = "tlam.tlambda"() ({
// BETA:   ^bb0(%1: !tlam.type):
// BETA:     %2 = "builtin.unrealized_conversion_cast"() : () -> i64
// BETA:     "tlam.treturn"(%2) : (i64) -> ()
// BETA:   }) : () -> !tlam.forall<i64>
// BETA:   %1 = "tlam.tlambda"() ({
// BETA:   ^bb0(%2: !tlam.type):
// BETA:     %3 = "tlam.vlambda"() ({
// BETA:     ^bb1(%4: !value<%2>):
// BETA:       "tlam.vreturn"(%4) : (!value<%2>) -> ()
// BETA:     }) : () -> !tlam.fun<!value<%2>, !value<%2>>
// BETA:     "tlam.treturn"(%3) : (!tlam.fun<!value<%2>, !value<%2>>) -> ()
// BETA:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// BETA:   %2 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// BETA:   %3 = "arith.constant"() <{value = 7}> : () -> i64
// BETA:   %4 = "tlam.vapply"(%2, %3) : (!tlam.fun<i64, i64>, i64) -> i64
// BETA:   "test.use"(%4) : (i64) -> ()
// BETA:   %5 = "tlam.vlambda"() ({
// BETA:   ^bb0(%6: i64):
// BETA:     "tlam.vreturn"(%6) : (i64) -> ()
// BETA:   }) : () -> !tlam.fun<i64, i64>
// BETA:   %6 = "arith.constant"() <{value = 11}> : () -> i64
// BETA:   "test.use"(%6) : (i64) -> ()
// BETA: }

// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:   ^bb0(%1: !tlam.type):
// MONO:     %2 = "builtin.unrealized_conversion_cast"() : () -> i64
// MONO:     "tlam.treturn"(%2) : (i64) -> ()
// MONO:   }) : () -> !tlam.forall<i64>
// MONO:   %1 = "tlam.tlambda"() ({
// MONO:   ^bb0(%2: !tlam.type):
// MONO:     %3 = "tlam.vlambda"() ({
// MONO:     ^bb1(%4: !value<%2>):
// MONO:       "tlam.vreturn"(%4) : (!value<%2>) -> ()
// MONO:     }) : () -> !tlam.fun<!value<%2>, !value<%2>>
// MONO:     "tlam.treturn"(%3) : (!tlam.fun<!value<%2>, !value<%2>>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// MONO:   %2 = "tlam.vlambda"() ({
// MONO:   ^bb0(%3: i64):
// MONO:     "tlam.vreturn"(%3) : (i64) -> ()
// MONO:   }) : () -> !tlam.fun<i64, i64>
// MONO:   %3 = "arith.constant"() <{value = 7}> : () -> i64
// MONO:   %4 = "tlam.vapply"(%2, %3) : (!tlam.fun<i64, i64>, i64) -> i64
// MONO:   "test.use"(%4) : (i64) -> ()
// MONO:   %5 = "tlam.vlambda"() ({
// MONO:   ^bb0(%6: i64):
// MONO:     "tlam.vreturn"(%6) : (i64) -> ()
// MONO:   }) : () -> !tlam.fun<i64, i64>
// MONO:   %6 = "arith.constant"() <{value = 11}> : () -> i64
// MONO:   "test.use"(%6) : (i64) -> ()
// MONO: }

// ERASE: builtin.module {
// ERASE:   %0 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%1: i64):
// ERASE:     "tlam.vreturn"(%1) : (i64) -> ()
// ERASE:   }) : () -> !tlam.fun<i64, i64>
// ERASE:   %1 = "arith.constant"() <{value = 7}> : () -> i64
// ERASE:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i64, i64>, i64) -> i64
// ERASE:   "test.use"(%2) : (i64) -> ()
// ERASE:   %3 = "arith.constant"() <{value = 11}> : () -> i64
// ERASE:   "test.use"(%3) : (i64) -> ()
// ERASE: }

// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i64) -> i64 {
// LOWER:     func.return %0 : i64
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i64) -> i64
// LOWER:   %1 = "arith.constant"() <{value = 7}> : () -> i64
// LOWER:   %2 = "func.call_indirect"(%0, %1) : ((i64) -> i64, i64) -> i64
// LOWER:   "test.use"(%2) : (i64) -> ()
// LOWER:   %3 = "arith.constant"() <{value = 11}> : () -> i64
// LOWER:   "test.use"(%3) : (i64) -> ()
// LOWER: }

// -----

// Example 2
builtin.module {
  %dead = "tlam.tlambda"() ({
  ^bb0(%T_dead: !tlam.type):
    %dead_i64 = "builtin.unrealized_conversion_cast"() : () -> i64
    "tlam.treturn"(%dead_i64) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %G = "tlam.tlambda"() ({
    ^bb0(%U: !tlam.type):
      %v = "tlam.vlambda"() ({
      ^bb0(%x: !value<%U>):
        "tlam.vreturn"(%x) : (!value<%U>) -> ()
      }) : () -> !tlam.fun<!value<%U>, !value<%U>>
      "tlam.treturn"(%v)
        : (!tlam.fun<!value<%U>, !value<%U>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

    %h = "tlam.tapply"(%G) <{tyArg = !value<%T>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
        -> !tlam.fun<!value<%T>, !value<%T>>

    "tlam.treturn"(%h)
      : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %f_i64 = "tlam.tapply"(%F) <{tyArg = i64}>
    : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
      -> !tlam.fun<i64, i64>

  %c = "arith.constant"() <{value = 9 : i64}> : () -> i64
  %r = "tlam.vapply"(%f_i64, %c) : (!tlam.fun<i64, i64>, i64) -> i64
  "test.use"(%r) : (i64) -> ()

  %id_inline = "tlam.vlambda"() ({
  ^bb0(%x: i64):
    "tlam.vreturn"(%x) : (i64) -> ()
  }) : () -> !tlam.fun<i64, i64>
  %beta_c = "arith.constant"() <{value = 13 : i64}> : () -> i64
  %beta_r = "tlam.vapply"(%id_inline, %beta_c) : (!tlam.fun<i64, i64>, i64) -> i64
  "test.use"(%beta_r) : (i64) -> ()
}

// BETA: // -----
// BETA: builtin.module {
// BETA:   %0 = "tlam.tlambda"() ({
// BETA:   ^bb0(%1: !tlam.type):
// BETA:     %2 = "builtin.unrealized_conversion_cast"() : () -> i64
// BETA:     "tlam.treturn"(%2) : (i64) -> ()
// BETA:   }) : () -> !tlam.forall<i64>
// BETA:   %1 = "tlam.tlambda"() ({
// BETA:   ^bb0(%2: !tlam.type):
// BETA:     %3 = "tlam.tlambda"() ({
// BETA:     ^bb1(%4: !tlam.type):
// BETA:       %5 = "tlam.vlambda"() ({
// BETA:       ^bb2(%6: !value<%4>):
// BETA:         "tlam.vreturn"(%6) : (!value<%4>) -> ()
// BETA:       }) : () -> !tlam.fun<!value<%4>, !value<%4>>
// BETA:       "tlam.treturn"(%5) : (!tlam.fun<!value<%4>, !value<%4>>) -> ()
// BETA:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// BETA:     %4 = "tlam.tapply"(%3) <{tyArg = !value<%2>}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<!value<%2>, !value<%2>>
// BETA:     "tlam.treturn"(%4) : (!tlam.fun<!value<%2>, !value<%2>>) -> ()
// BETA:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// BETA:   %2 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// BETA:   %3 = "arith.constant"() <{value = 9}> : () -> i64
// BETA:   %4 = "tlam.vapply"(%2, %3) : (!tlam.fun<i64, i64>, i64) -> i64
// BETA:   "test.use"(%4) : (i64) -> ()
// BETA:   %5 = "tlam.vlambda"() ({
// BETA:   ^bb0(%6: i64):
// BETA:     "tlam.vreturn"(%6) : (i64) -> ()
// BETA:   }) : () -> !tlam.fun<i64, i64>
// BETA:   %6 = "arith.constant"() <{value = 13}> : () -> i64
// BETA:   "test.use"(%6) : (i64) -> ()
// BETA: }

// MONO: // -----
// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:   ^bb0(%1: !tlam.type):
// MONO:     %2 = "builtin.unrealized_conversion_cast"() : () -> i64
// MONO:     "tlam.treturn"(%2) : (i64) -> ()
// MONO:   }) : () -> !tlam.forall<i64>
// MONO:   %1 = "tlam.tlambda"() ({
// MONO:   ^bb0(%2: !tlam.type):
// MONO:     %3 = "tlam.tlambda"() ({
// MONO:     ^bb1(%4: !tlam.type):
// MONO:       %5 = "tlam.vlambda"() ({
// MONO:       ^bb2(%6: !value<%4>):
// MONO:         "tlam.vreturn"(%6) : (!value<%4>) -> ()
// MONO:       }) : () -> !tlam.fun<!value<%4>, !value<%4>>
// MONO:       "tlam.treturn"(%5) : (!tlam.fun<!value<%4>, !value<%4>>) -> ()
// MONO:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// MONO:     %4 = "tlam.vlambda"() ({
// MONO:     ^bb1(%5: !value<%2>):
// MONO:       "tlam.vreturn"(%5) : (!value<%2>) -> ()
// MONO:     }) : () -> !tlam.fun<!value<%2>, !value<%2>>
// MONO:     "tlam.treturn"(%4) : (!tlam.fun<!value<%2>, !value<%2>>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// MONO:   %2 = "tlam.tlambda"() ({
// MONO:   ^bb0(%3: !tlam.type):
// MONO:     %4 = "tlam.vlambda"() ({
// MONO:     ^bb1(%5: !value<%3>):
// MONO:       "tlam.vreturn"(%5) : (!value<%3>) -> ()
// MONO:     }) : () -> !tlam.fun<!value<%3>, !value<%3>>
// MONO:     "tlam.treturn"(%4) : (!tlam.fun<!value<%3>, !value<%3>>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// MONO:   %3 = "tlam.vlambda"() ({
// MONO:   ^bb0(%4: i64):
// MONO:     "tlam.vreturn"(%4) : (i64) -> ()
// MONO:   }) : () -> !tlam.fun<i64, i64>
// MONO:   %4 = "arith.constant"() <{value = 9}> : () -> i64
// MONO:   %5 = "tlam.vapply"(%3, %4) : (!tlam.fun<i64, i64>, i64) -> i64
// MONO:   "test.use"(%5) : (i64) -> ()
// MONO:   %6 = "tlam.vlambda"() ({
// MONO:   ^bb0(%7: i64):
// MONO:     "tlam.vreturn"(%7) : (i64) -> ()
// MONO:   }) : () -> !tlam.fun<i64, i64>
// MONO:   %7 = "arith.constant"() <{value = 13}> : () -> i64
// MONO:   "test.use"(%7) : (i64) -> ()
// MONO: }

// ERASE: // -----
// ERASE: builtin.module {
// ERASE:   %0 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%1: i64):
// ERASE:     "tlam.vreturn"(%1) : (i64) -> ()
// ERASE:   }) : () -> !tlam.fun<i64, i64>
// ERASE:   %1 = "arith.constant"() <{value = 9}> : () -> i64
// ERASE:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i64, i64>, i64) -> i64
// ERASE:   "test.use"(%2) : (i64) -> ()
// ERASE:   %3 = "arith.constant"() <{value = 13}> : () -> i64
// ERASE:   "test.use"(%3) : (i64) -> ()
// ERASE: }

// LOWER: // -----
// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i64) -> i64 {
// LOWER:     func.return %0 : i64
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i64) -> i64
// LOWER:   %1 = "arith.constant"() <{value = 9}> : () -> i64
// LOWER:   %2 = "func.call_indirect"(%0, %1) : ((i64) -> i64, i64) -> i64
// LOWER:   "test.use"(%2) : (i64) -> ()
// LOWER:   %3 = "arith.constant"() <{value = 13}> : () -> i64
// LOWER:   "test.use"(%3) : (i64) -> ()
// LOWER: }

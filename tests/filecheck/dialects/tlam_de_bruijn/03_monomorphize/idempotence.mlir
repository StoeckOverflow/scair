// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize | filecheck %s --check-prefix=MONO
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,monomorphize | filecheck %s --check-prefix=MONO2

// VALID: monomorphize removes tapply and is stable on second run.
builtin.module {
  %outer = "tlam.tlambda"() ({
    %poly_id = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    %a = "tlam.tapply"(%poly_id) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    %b = "tlam.tapply"(%poly_id) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%a) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}
// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:     %1 = "tlam.tlambda"() ({
// MONO:       %2 = "tlam.vlambda"() ({
// MONO:       ^bb0(%3: !tlam.bvar<0>):
// MONO:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// MONO:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// MONO:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// MONO:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// MONO:     %2 = "tlam.vlambda"() ({
// MONO:     ^bb0(%3: i64):
// MONO:       "tlam.vreturn"(%3) : (i64) -> ()
// MONO:     }) : () -> !tlam.fun<i64, i64>
// MONO:     "tlam.treturn"(%2) : (!tlam.fun<i64, i64>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// MONO: }
// MONO2: builtin.module {
// MONO2:   %0 = "tlam.tlambda"() ({
// MONO2:     %1 = "tlam.tlambda"() ({
// MONO2:       %2 = "tlam.vlambda"() ({
// MONO2:       ^bb0(%3: !tlam.bvar<0>):
// MONO2:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// MONO2:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// MONO2:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// MONO2:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// MONO2:     %2 = "tlam.vlambda"() ({
// MONO2:     ^bb0(%3: i64):
// MONO2:       "tlam.vreturn"(%3) : (i64) -> ()
// MONO2:     }) : () -> !tlam.fun<i64, i64>
// MONO2:     "tlam.treturn"(%2) : (!tlam.fun<i64, i64>) -> ()
// MONO2:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// MONO2: }

// -----

// INVALID: monomorphize should report verifier error, not crash.
builtin.module {
  // expected-error @below {{debruijn: bvar<3> out of scope at depth=1}}
  %bad = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<3>):
      "tlam.vreturn"(%x) : (!tlam.bvar<3>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>>)
}

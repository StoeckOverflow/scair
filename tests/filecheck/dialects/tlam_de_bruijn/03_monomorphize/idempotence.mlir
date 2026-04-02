// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn | filecheck %s --check-prefix=MONO
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,monomorphize-tlam-de-bruijn | filecheck %s --check-prefix=MONO2

// VALID: monomorphize removes tapply and is stable on second run.
builtin.module {
  %outer = "tlam_dbi.tlambda"() ({
    %poly_id = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

    %a = "tlam_dbi.tapply"(%poly_id) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    %b = "tlam_dbi.tapply"(%poly_id) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    "tlam_dbi.treturn"(%a) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
}
// MONO: builtin.module {
// MONO:   %0 = "tlam_dbi.tlambda"() ({
// MONO:     %1 = "tlam_dbi.tlambda"() ({
// MONO:       %2 = "tlam_dbi.vlambda"() ({
// MONO:       ^bb0(%3: !tlam_dbi.bvar<0>):
// MONO:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// MONO:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// MONO:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// MONO:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// MONO:     %2 = "tlam_dbi.vlambda"() ({
// MONO:     ^bb0(%3: i64):
// MONO:       "tlam_dbi.vreturn"(%3) : (i64) -> ()
// MONO:     }) : () -> !tlam_dbi.fun<i64, i64>
// MONO:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// MONO: }
// MONO2: builtin.module {
// MONO2:   %0 = "tlam_dbi.tlambda"() ({
// MONO2:     %1 = "tlam_dbi.tlambda"() ({
// MONO2:       %2 = "tlam_dbi.vlambda"() ({
// MONO2:       ^bb0(%3: !tlam_dbi.bvar<0>):
// MONO2:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// MONO2:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// MONO2:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// MONO2:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// MONO2:     %2 = "tlam_dbi.vlambda"() ({
// MONO2:     ^bb0(%3: i64):
// MONO2:       "tlam_dbi.vreturn"(%3) : (i64) -> ()
// MONO2:     }) : () -> !tlam_dbi.fun<i64, i64>
// MONO2:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO2:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// MONO2: }

// -----

// INVALID: monomorphize should report verifier error, not crash.
builtin.module {
  // expected-error @below {{debruijn-dbi: bvar<3> out of scope at depth=1}}
  %bad = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<3>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<3>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>>)
}

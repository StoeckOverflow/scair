// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn | filecheck %s -DFILE=%s --check-prefix=MONO
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce | filecheck %s -DFILE=%s --check-prefix=DCE

// Monomorphize should reuse one specialization for three identical tapply sites.
builtin.module {
  %outer = "tlam_dbi.tlambda"() ({
    %poly = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

    %a = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    %b = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    %c = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    "test.use"(%a) : (!tlam_dbi.fun<i64, i64>) -> ()
    "test.use"(%b) : (!tlam_dbi.fun<i64, i64>) -> ()
    "test.use"(%c) : (!tlam_dbi.fun<i64, i64>) -> ()
    "tlam_dbi.treturn"(%a) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
  "test.use"(%outer) : (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>) -> ()
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
// MONO:     "test.use"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO:     "test.use"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO:     "test.use"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// MONO:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// MONO:   "test.use"(%0) : (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>) -> ()
// MONO: }
// DCE: builtin.module {
// DCE:   %0 = "tlam_dbi.tlambda"() ({
// DCE:     %1 = "tlam_dbi.vlambda"() ({
// DCE:     ^bb0(%2: i64):
// DCE:       "tlam_dbi.vreturn"(%2) : (i64) -> ()
// DCE:     }) : () -> !tlam_dbi.fun<i64, i64>
// DCE:     "test.use"(%1) : (!tlam_dbi.fun<i64, i64>) -> ()
// DCE:     "test.use"(%1) : (!tlam_dbi.fun<i64, i64>) -> ()
// DCE:     "test.use"(%1) : (!tlam_dbi.fun<i64, i64>) -> ()
// DCE:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.fun<i64, i64>) -> ()
// DCE:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// DCE:   "test.use"(%0) : (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>) -> ()
// DCE: }

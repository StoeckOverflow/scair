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
// MONO-NOT: "tlam.tapply"
// MONO: !tlam.fun<i64, i64>
// MONO2-NOT: "tlam.tapply"
// MONO2: !tlam.fun<i64, i64>

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

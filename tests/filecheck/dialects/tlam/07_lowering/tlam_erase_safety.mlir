// Purpose: Erase pass safety on non-monomorphized or malformed TLam input.
// Invariants covered: erase-tlam does not destructively rewrite live/malformed tlambda.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p erase-tlam --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=ERASESAFE

// Live tlambda (still used by tapply) must not be erased by erase-tlam.
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> !tlam.fun<i32, i32>
    "tlam.treturn"(%id) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<i32, i32>>

  %f = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.fun<i32, i32>>) -> !tlam.fun<i32, i32>
  "test.use"(%f) : (!tlam.fun<i32, i32>) -> ()
}

// ERASESAFE-LABEL: builtin.module {
// ERASESAFE: "tlam.tlambda"
// ERASESAFE: "tlam.tapply"
// ERASESAFE: }

// -----

// Malformed tlambda must be left unchanged; verifier reports shape error.
// expected-error @below {{tlambda: last op must be tlam.treturn}}
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> !tlam.fun<i32, i32>
    "test.use"(%id) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<i32, i32>>
  "test.use"(%mk) : (!tlam.forall<!tlam.fun<i32, i32>>) -> ()
}

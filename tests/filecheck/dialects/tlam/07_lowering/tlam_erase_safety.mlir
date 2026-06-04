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

// ERASESAFE: builtin.module {
// ERASESAFE:   %0 = "tlam.tlambda"() ({
// ERASESAFE:   ^bb0(%1: !tlam.type):
// ERASESAFE:     %2 = "tlam.vlambda"() ({
// ERASESAFE:     ^bb1(%3: i32):
// ERASESAFE:       "tlam.vreturn"(%3) : (i32) -> ()
// ERASESAFE:     }) : () -> !tlam.fun<i32, i32>
// ERASESAFE:     "tlam.treturn"(%2) : (!tlam.fun<i32, i32>) -> ()
// ERASESAFE:   }) : () -> !tlam.forall<!tlam.fun<i32, i32>>
// ERASESAFE:   %1 = "tlam.tapply"(%0) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<i32, i32>>) -> !tlam.fun<i32, i32>
// ERASESAFE:   "test.use"(%1) : (!tlam.fun<i32, i32>) -> ()
// ERASESAFE: }

// -----

// Dead tlambda must not be erased if moving the body would leak the binder through
// operands, result types, or nested attribute payloads.
builtin.module {
  %dead = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %tv = "builtin.unrealized_conversion_cast"(%T)
        {dep = !tlam.forall<!value<%T>>}
        : (!tlam.type) -> !value<%T>
    %v = "test.make_i64"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>
}

// ERASESAFE: builtin.module {
// ERASESAFE:   %0 = "tlam.tlambda"() ({
// ERASESAFE:   ^bb0(%1: !tlam.type):
// ERASESAFE:     %2 = "builtin.unrealized_conversion_cast"(%1) {dep = !tlam.forall<!value<%1>>} : (!tlam.type) -> !value<%1>
// ERASESAFE:     %3 = "test.make_i64"() : () -> i64
// ERASESAFE:     "tlam.treturn"(%3) : (i64) -> ()
// ERASESAFE:   }) : () -> !tlam.forall<i64>
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

// ERASESAFE: tlambda: last op must be tlam.treturn

// Purpose: Verifier checks for de Bruijn index well-scoping.
// Invariants covered: Bvar bounds are enforced under binder depth and valid forms pass.

// RUN: scair-opt %s --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Valid: bvar<0> under one binder.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
    "tlam.treturn"(%v)
      : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
}

// CHECK-LABEL: builtin.module {
// CHECK: "tlam.tlambda"()
// CHECK: "tlam.vlambda"()
// CHECK: "tlam.vreturn"
// CHECK: "tlam.treturn"
// CHECK: }

// -----

// Invalid: bvar<0> at top-level depth 0.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.bvar<0>):
    "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
  }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
}

// CHECK: debruijn: bvar<0> out of scope at depth=0

// -----

// Invalid: bvar<1> under a single binder.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.op"() : () -> !tlam.bvar<1>
    "tlam.treturn"(%v) : (!tlam.bvar<1>) -> ()
  }) : () -> !tlam.forall<!tlam.bvar<1>>
}

// CHECK: debruijn: bvar<1> out of scope at depth=1

// -----

// Invalid: bvar<1> in top-level tapply tyArg forall body.
builtin.module {
  %poly = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%T>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %bad = "tlam.tapply"(%poly) <{tyArg = !tlam.forall<!tlam.bvar<1>>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
       -> !tlam.fun<!tlam.forall<!tlam.bvar<1>>, !tlam.forall<!tlam.bvar<1>>>
}

// CHECK: debruijn: bvar<1> out of scope at depth=1

// -----

// Valid: bvar<1> is in scope inside a forall under an outer tlambda binder.
builtin.module {
  %poly = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%T>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %outer = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %good = "tlam.tapply"(%poly) <{tyArg = !tlam.forall<!tlam.bvar<1>>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
       -> !tlam.fun<!tlam.forall<!tlam.bvar<1>>, !tlam.forall<!tlam.bvar<1>>>
    %v = "tlam.vlambda"() ({
    ^bb1(%x: !tlam.tvar<%U>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%U>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>
    "tlam.treturn"(%v) : (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
}

// CHECK-LABEL: builtin.module {
// CHECK: "tlam.tlambda"()
// CHECK: "tlam.tapply"
// CHECK: "tlam.treturn"
// CHECK: }

// -----

// Invalid: top-level tapply tyArg bvar<0> at depth=0.
builtin.module {
  %poly = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%T>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %bad = "tlam.tapply"(%poly) <{tyArg = !tlam.bvar<0>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
       -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
}

// CHECK: debruijn: bvar<0> out of scope at depth=0

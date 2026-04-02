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

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:   ^bb0(%1: !tlam.type):
// CHECK:     %2 = "tlam.vlambda"() ({
// CHECK:     ^bb1(%3: !tlam.bvar<0>):
// CHECK:       "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// CHECK:     }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// CHECK:     "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
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
    ^bb1(%x: !value<%T>):
      "tlam.vreturn"(%x) : (!value<%T>) -> ()
    }) : () -> !tlam.fun<!value<%T>, !value<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
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
    ^bb1(%x: !value<%T>):
      "tlam.vreturn"(%x) : (!value<%T>) -> ()
    }) : () -> !tlam.fun<!value<%T>, !value<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %outer = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %good = "tlam.tapply"(%poly) <{tyArg = !tlam.forall<!tlam.bvar<1>>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
       -> !tlam.fun<!tlam.forall<!tlam.bvar<1>>, !tlam.forall<!tlam.bvar<1>>>
    %v = "tlam.vlambda"() ({
    ^bb1(%x: !value<%U>):
      "tlam.vreturn"(%x) : (!value<%U>) -> ()
    }) : () -> !tlam.fun<!value<%U>, !value<%U>>
    "tlam.treturn"(%v) : (!tlam.fun<!value<%U>, !value<%U>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:   ^bb0(%1: !tlam.type):
// CHECK:     %2 = "tlam.vlambda"() ({
// CHECK:     ^bb1(%3: !value<%1>):
// CHECK:       "tlam.vreturn"(%3) : (!value<%1>) -> ()
// CHECK:     }) : () -> !tlam.fun<!value<%1>, !value<%1>>
// CHECK:     "tlam.treturn"(%2) : (!tlam.fun<!value<%1>, !value<%1>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK:   %1 = "tlam.tlambda"() ({
// CHECK:   ^bb0(%2: !tlam.type):
// CHECK:     %3 = "tlam.tapply"(%0) <{tyArg = !tlam.forall<!tlam.bvar<1>>}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<!tlam.forall<!tlam.bvar<1>>, !tlam.forall<!tlam.bvar<1>>>
// CHECK:     %4 = "tlam.vlambda"() ({
// CHECK:     ^bb1(%5: !value<%2>):
// CHECK:       "tlam.vreturn"(%5) : (!value<%2>) -> ()
// CHECK:     }) : () -> !tlam.fun<!value<%2>, !value<%2>>
// CHECK:     "tlam.treturn"(%4) : (!tlam.fun<!value<%2>, !value<%2>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK: }

// -----

// Invalid: top-level tapply tyArg bvar<0> at depth=0.
builtin.module {
  %poly = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: !value<%T>):
      "tlam.vreturn"(%x) : (!value<%T>) -> ()
    }) : () -> !tlam.fun<!value<%T>, !value<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %bad = "tlam.tapply"(%poly) <{tyArg = !tlam.bvar<0>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
       -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
}

// CHECK: debruijn: bvar<0> out of scope at depth=0

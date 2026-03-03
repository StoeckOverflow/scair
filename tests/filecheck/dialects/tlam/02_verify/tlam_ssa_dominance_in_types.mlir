// Purpose: Verifier dominance checks for SSA references embedded in TLam types.
// Invariants covered: tvar references must dominate all use sites in type positions.

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Passing case: tvar uses are dominated within the tlambda body.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "tlam.vlambda"() ({
    ^bb0(%x: !value<%T>):
      "tlam.vreturn"(%x) : (!value<%T>) -> ()
    }) : () -> !tlam.fun<!value<%T>, !value<%T>>
    "tlam.treturn"(%v)
      : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
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
// CHECK: }

// -----

// Use-before-def of a type value in an attribute.
builtin.module {
  "test.use"() {dep = !value<%T>} : () -> ()
  %T = "test.make_type"() : () -> !tlam.type
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate its use in op `test.use`

// -----

// Use-before-def of a type value in a tapply type argument.
builtin.module {
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

  %T = "test.make_type"() : () -> !tlam.type
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate its use in op `tlam.tapply`

// -----

// Passing case: tvar may reference any dominating !tlam.type value (not only a tlambda binder).
builtin.module {
  %T = "test.make_type"() : () -> !tlam.type
  "test.use"() {dep = !value<%T>} : () -> ()
}

// CHECK: builtin.module {
// CHECK:   %0 = "test.make_type"() : () -> !tlam.type
// CHECK:   "test.use"() {dep = !value<%0>} : () -> ()
// CHECK: }

// -----

// Use-before-def only in tapply.tyArg property (result type has no tvar).
builtin.module {
  %G = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %v = "test.make_i64"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %h = "tlam.tapply"(%G) <{tyArg = !value<%T>}>
       : (!tlam.forall<i64>) -> i64

  %T = "test.make_type"() : () -> !tlam.type
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate its use in op `tlam.tapply`

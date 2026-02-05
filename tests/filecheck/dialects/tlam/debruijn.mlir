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

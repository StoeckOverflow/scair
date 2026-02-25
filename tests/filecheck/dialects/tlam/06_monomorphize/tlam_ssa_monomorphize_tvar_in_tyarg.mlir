// Purpose: Monomorphize substitution with type arguments that themselves contain Tvar.
// Invariants covered: capture-avoiding DBI substitution with nested binders plus preservation of embedded Tvar SSA identity.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=MONO

// Valid: instantiate a forall with tyArg = forall<tvar<%Y>> and keep embedded %Y identity.
builtin.module {
  %x = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %Y = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type

  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.mk_poly2"() : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !value<%T>>>
    "tlam.treturn"(%v) : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !value<%T>>>) -> ()
  }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<1>>>>

  %spec = "tlam.tapply"(%mk) <{tyArg = !tlam.forall<!value<%Y>>}>
      : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<1>>>>)
       -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.forall<!value<%Y>>>>
  "test.use"(%spec) : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.forall<!value<%Y>>>>) -> ()
}

// MONO-LABEL: builtin.module {
// MONO: [[Y:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// MONO-NOT: "tlam.tapply"
// MONO: "test.mk_poly2"() : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.forall<!value<[[Y]]>>>>
// MONO: "test.use"(%{{[0-9]+}}) : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.forall<!value<[[Y]]>>>>) -> ()

// -----

// Invalid: instantiated type mismatch is still rejected for tyArg containing tvar.
// expected-error @below {{tapply: result}}
builtin.module {
  %x = "arith.constant"() <{value = 9 : i32}> : () -> i32
  %Y = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type

  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.mk_poly2"() : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !value<%T>>>
    "tlam.treturn"(%v) : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !value<%T>>>) -> ()
  }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<1>>>>

  %bad = "tlam.tapply"(%mk) <{tyArg = !tlam.forall<!value<%Y>>}>
      : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<1>>>>)
       -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, i32>>
}

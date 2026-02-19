// Purpose: Monomorphize behavior with SSA-in-types and nested de Bruijn binders.
// Invariants covered: Repeated specialization, capture-avoiding DBI substitution, and verifier safety.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=MONO

// Targets: monomorphize with repeated specialization + nested binder DBI behavior.

// Valid: repeated same specialization and nested forall under tlambda.
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.mk_poly"() : () -> !tlam.forall<!tlam.tvar<%T>>
    "tlam.treturn"(%v) : (!tlam.forall<!tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>

  %s0 = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<i32>
  %s1 = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<i32>

  "test.use"(%s0, %s1) : (!tlam.forall<i32>, !tlam.forall<i32>) -> ()
}

// MONO-LABEL: builtin.module {
// MONO-NOT: "tlam.tapply"
// MONO: "test.mk_poly"() : () -> !tlam.forall<i32>
// MONO: "test.use"(%{{[0-9]+}}, %{{[0-9]+}}) : (!tlam.forall<i32>, !tlam.forall<i32>) -> ()
// MONO: }

// -----

// Valid: one polymorphic value instantiated at two different tyArgs.
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.mk_poly"() : () -> !tlam.forall<!tlam.tvar<%T>>
    "tlam.treturn"(%v) : (!tlam.forall<!tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>

  %s32 = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<i32>
  %s64 = "tlam.tapply"(%mk) <{tyArg = i64}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<i64>

  "test.use"(%s32, %s64) : (!tlam.forall<i32>, !tlam.forall<i64>) -> ()
}

// MONO-LABEL: builtin.module {
// MONO-NOT: "tlam.tapply"
// MONO: "test.mk_poly"() : () -> !tlam.forall<i32>
// MONO: "test.mk_poly"() : () -> !tlam.forall<i64>
// MONO: "test.use"(%{{[0-9]+}}, %{{[0-9]+}}) : (!tlam.forall<i32>, !tlam.forall<i64>) -> ()
// MONO: }

// -----

// Invalid: instantiation result mismatch must be rejected.
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.mk_poly"() : () -> !tlam.forall<!tlam.tvar<%T>>
    "tlam.treturn"(%v) : (!tlam.forall<!tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>

  %bad = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<!tlam.bvar<0>>
}

// MONO: tapply: result !tlam.forall<!tlam.bvar<0>> != instantiated !tlam.forall<i32>

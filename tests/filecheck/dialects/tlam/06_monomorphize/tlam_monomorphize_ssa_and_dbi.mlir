// Purpose: Monomorphize behavior with SSA-in-types and nested de Bruijn binders.
// Invariants covered: Repeated specialization, capture-avoiding DBI substitution, and verifier safety.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p monomorphize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=MONO

// Targets: monomorphize with repeated specialization + nested binder DBI behavior.

// Valid: repeated same specialization and nested forall under tlambda.
// Because test.mk_poly is not known effect-free, identical specializations are cloned rather than reused.
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.mk_poly"() : () -> !tlam.forall<!value<%T>>
    "tlam.treturn"(%v) : (!tlam.forall<!value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>

  %s0 = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<i32>
  %s1 = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<i32>

  "test.use"(%s0, %s1) : (!tlam.forall<i32>, !tlam.forall<i32>) -> ()
}

// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:   ^bb0(%1: !tlam.type):
// MONO:     %2 = "test.mk_poly"() : () -> !tlam.forall<!value<%1>>
// MONO:     "tlam.treturn"(%2) : (!tlam.forall<!value<%1>>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>
// MONO:   %1 = "test.mk_poly"() : () -> !tlam.forall<i32>
// MONO:   %2 = "test.mk_poly"() : () -> !tlam.forall<i32>
// MONO:   "test.use"(%1, %2) : (!tlam.forall<i32>, !tlam.forall<i32>) -> ()
// MONO: }

// -----

// Valid: one polymorphic value instantiated at two different tyArgs.
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.mk_poly"() : () -> !tlam.forall<!value<%T>>
    "tlam.treturn"(%v) : (!tlam.forall<!value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>

  %s32 = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<i32>
  %s64 = "tlam.tapply"(%mk) <{tyArg = i64}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<i64>

  "test.use"(%s32, %s64) : (!tlam.forall<i32>, !tlam.forall<i64>) -> ()
}

// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:   ^bb0(%1: !tlam.type):
// MONO:     %2 = "test.mk_poly"() : () -> !tlam.forall<!value<%1>>
// MONO:     "tlam.treturn"(%2) : (!tlam.forall<!value<%1>>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>
// MONO:   %1 = "test.mk_poly"() : () -> !tlam.forall<i32>
// MONO:   %2 = "test.mk_poly"() : () -> !tlam.forall<i64>
// MONO:   "test.use"(%1, %2) : (!tlam.forall<i32>, !tlam.forall<i64>) -> ()
// MONO: }

// -----

// Valid: rewrite nested attribute trees and remap embedded SSA refs to cloned defs.
builtin.module {
  %x = "arith.constant"() <{value = 3 : i32}> : () -> i32
  %Y = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type

  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %tv = "builtin.unrealized_conversion_cast"(%T)
        {dep = !tlam.forall<!value<%T>>}
        : (!tlam.type) -> !tlam.type
    "test.use"(%tv)
        {dep = !tlam.forall<!tlam.fun<!value<%tv>, !tlam.forall<!value<%T>>>>}
        : (!tlam.type) -> ()
    "tlam.treturn"(%tv) : (!tlam.type) -> ()
  }) : () -> !tlam.forall<!tlam.type>

  %spec = "tlam.tapply"(%mk) <{tyArg = !value<%Y>}>
      : (!tlam.forall<!tlam.type>) -> !tlam.type
  "test.consume"(%spec) : (!tlam.type) -> ()
}

// MONO: builtin.module {
// MONO:   %0 = "arith.constant"() <{value = 3 : i32}> : () -> i32
// MONO:   %1 = "builtin.unrealized_conversion_cast"(%0) : (i32) -> !tlam.type
// MONO:   %2 = "tlam.tlambda"() ({
// MONO:   ^bb0(%3: !tlam.type):
// MONO:     %4 = "builtin.unrealized_conversion_cast"(%3) {dep = !tlam.forall<!value<%3>>} : (!tlam.type) -> !tlam.type
// MONO:     "test.use"(%4) {dep = !tlam.forall<!tlam.fun<!value<%4>, !tlam.forall<!value<%3>>>>} : (!tlam.type) -> ()
// MONO:     "tlam.treturn"(%4) : (!tlam.type) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.type>
// MONO:   %3 = "builtin.unrealized_conversion_cast"(%1) {dep = !tlam.forall<!value<%1>>} : (!tlam.type) -> !tlam.type
// MONO:   "test.use"(%3) {dep = !tlam.forall<!tlam.fun<!value<%3>, !tlam.forall<!value<%1>>>>} : (!tlam.type) -> ()
// MONO:   "test.consume"(%3) : (!tlam.type) -> ()
// MONO: }

// -----

// Invalid: instantiation result mismatch must be rejected.
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.mk_poly"() : () -> !tlam.forall<!value<%T>>
    "tlam.treturn"(%v) : (!tlam.forall<!value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>

  %bad = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<!tlam.bvar<0>>
}

// MONO: tapply: result !tlam.forall<!tlam.bvar<0>> != instantiated !tlam.forall<i32>

// -----

// Invalid shape: malformed tlambda must not crash monomorphize.
// expected-error @below {{tlambda: last op must be tlam.treturn}}
builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.mk_poly"() : () -> !tlam.forall<!value<%T>>
    "test.use"(%v) : (!tlam.forall<!value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.forall<!tlam.bvar<1>>>

  %s0 = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.forall<!tlam.bvar<1>>>) -> !tlam.forall<i32>
  "test.use"(%s0) : (!tlam.forall<i32>) -> ()
}

// MONO: tlambda: last op must be tlam.treturn

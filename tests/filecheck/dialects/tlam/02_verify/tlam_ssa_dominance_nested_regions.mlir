// Purpose: Dominance-in-types coverage for nested regions under rewrite-like verification paths.
// Invariants covered: outer type defs dominate nested-region Tvar uses; nested forward refs are rejected.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=VERIFY
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize,cse --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=PIPE

// Valid: outer %T dominates nested region type use.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %T = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type

  "scf.execute_region"() ({
  ^bb0:
    "test.use"() {dep = !tlam.forall<!tlam.tvar<%T>>} : () -> ()
    "scf.yield"() : () -> ()
  }) : () -> ()
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// VERIFY: "scf.execute_region"() ({
// VERIFY: "test.use"() {dep = !tlam.forall<!tlam.tvar<%{{[0-9]+}}>>} : () -> ()
// VERIFY: }

// PIPE-LABEL: builtin.module {
// PIPE: "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// PIPE: "scf.execute_region"() ({
// PIPE: "test.use"() {dep = !tlam.forall<!tlam.tvar<%{{[0-9]+}}>>} : () -> ()
// PIPE: }

// -----

// Invalid: nested forward reference of type value fails dominance.
// expected-error @below {{ssa-dominance: value Value}}
builtin.module {
  "scf.execute_region"() ({
  ^bb0:
    "test.use"() {dep = !tlam.forall<!tlam.tvar<%T>>} : () -> ()
    %x = "arith.constant"() <{value = 1 : i32}> : () -> i32
    %T = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
    "scf.yield"() : () -> ()
  }) : () -> ()
}

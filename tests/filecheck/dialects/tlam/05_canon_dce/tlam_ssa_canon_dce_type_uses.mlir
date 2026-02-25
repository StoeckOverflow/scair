// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=CANON
// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p canonicalize,canonicalize --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=CANON
// Purpose: Canonicalization dead-op cleanup with SSA-in-types uses.
// Invariants covered: values used only through embedded Tvar are retained; no accidental DCE of type-only dependencies.

// Value used only in embedded tvar must not be removed.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %T = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  "test.use"() {dep = !tlam.forall<!value<%T>>} : () -> ()
}

// CANON-LABEL: builtin.module {
// CANON: [[T:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// CANON: "test.use"() {dep = !tlam.forall<!value<[[T]]>>} : () -> ()
// CANON: }

// -----

// Value used only via nested fun/forall tvars must be retained.
builtin.module {
  %x = "arith.constant"() <{value = 5 : i32}> : () -> i32
  %T = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  "test.use"() {dep = !tlam.forall<!tlam.fun<!value<%T>, !tlam.forall<!value<%T>>>>} : () -> ()
}

// CANON-LABEL: builtin.module {
// CANON: [[T:%[0-9]+]] = "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// CANON: "test.use"() {dep = !tlam.forall<!tlam.fun<!value<[[T]]>, !tlam.forall<!value<[[T]]>>>>} : () -> ()
// CANON: }

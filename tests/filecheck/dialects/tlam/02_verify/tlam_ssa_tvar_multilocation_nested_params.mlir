// Purpose: Deep Tvar embedding coverage across multiple IR locations.
// Invariants covered: AttributeWalker reaches embedded SSA-in-types references in result types, attrs, properties, and nested attribute lists for verifier checks.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=VERIFY

// Valid: one dominating type value is embedded in all target locations.
builtin.module {
  %T = "test.make_type"() : () -> !tlam.type
  %tv = "builtin.unrealized_conversion_cast"(%T) {
    dep = !tlam.forall<!tlam.fun<!value<%T>, !tlam.forall<!value<%T>>>>,
    nested = [!value<%T>, [!tlam.forall<!value<%T>>, !value<%T>]]
  } : (!tlam.type) -> !value<%T>
  "test.use"(%tv) : (!value<%T>) -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.make_type"() : () -> !tlam.type
// VERIFY:   %1 = "builtin.unrealized_conversion_cast"(%0) {dep = !tlam.forall<!tlam.fun<!value<%0>, !tlam.forall<!value<%0>>>>, nested = [!value<%0>, [!tlam.forall<!value<%0>>, !value<%0>]]} : (!tlam.type) -> !value<%0>
// VERIFY:   "test.use"(%1) : (!value<%0>) -> ()
// VERIFY: }

// -----

// Invalid nested attr forward reference in embedded tvar.
// expected-error @below {{ssa-dominance: value Value(!tlam.type) does not dominate its use in op `test.use`}}
builtin.module {
  "test.use"() {nested = [!value<%T>, [!tlam.forall<!value<%T>>, !value<%T>]]} : () -> ()
  %T = "test.make_type"() : () -> !tlam.type
}

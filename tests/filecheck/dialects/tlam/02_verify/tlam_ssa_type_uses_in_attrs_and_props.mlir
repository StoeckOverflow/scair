// Purpose: Dominance and traversal checks for tvar uses inside attrs/properties.
// Invariants covered: Attribute/property-embedded tvar uses are walked and dominance-verified.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=VERIFY

// Targets: dominance-in-types for Tvar in attributes + operation properties.

// Valid: dominating type value referenced from a nested attribute parameter.
builtin.module {
  %T = "test.make_type"() : () -> !tlam.type
  "test.use"() {dep = !tlam.forall<!tlam.fun<i32, !value<%T>>>} : () -> ()
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "test.make_type"() : () -> !tlam.type
// VERIFY: "test.use"() {dep = !tlam.forall<!tlam.fun<i32, !value<%{{[0-9]+}}>>>
// VERIFY: }

// -----

// Invalid: forward reference in attribute payload.
builtin.module {
  "test.use"() {dep = !tlam.forall<!tlam.fun<i32, !value<%T>>>} : () -> ()
  %T = "test.make_type"() : () -> !tlam.type
}

// VERIFY: ssa-dominance: value Value{{.*}} does not dominate its use in op `test.use`

// -----

// Valid: dominating type value referenced from tapply.tyArg property.
builtin.module {
  %T = "test.make_type"() : () -> !tlam.type
  %G = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %v = "test.make_i64"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %h = "tlam.tapply"(%G) <{tyArg = !value<%T>}>
       : (!tlam.forall<i64>) -> i64
}

// VERIFY-LABEL: builtin.module {
// VERIFY: "tlam.tapply"(%{{[0-9]+}}) <{tyArg = !value<%{{[0-9]+}}>}> : (!tlam.forall<i64>) -> i64
// VERIFY: }

// -----

// Invalid: forward reference in tapply.tyArg property.
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

// VERIFY: ssa-dominance: value Value{{.*}} does not dominate its use in op `tlam.tapply`

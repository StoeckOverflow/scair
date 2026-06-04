// Purpose: Dominance and traversal checks for tvar uses inside types/attrs/properties.
// Invariants covered: Embedded tvar uses are walked and dominance-verified.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=VERIFY

// Targets: dominance-in-types for Tvar in result types, attributes, and operation properties.

// Valid: tlambda binder dominates nested type uses within its body.
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

// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam.tlambda"() ({
// VERIFY:   ^bb0(%1: !tlam.type):
// VERIFY:     %2 = "tlam.vlambda"() ({
// VERIFY:     ^bb1(%3: !value<%1>):
// VERIFY:       "tlam.vreturn"(%3) : (!value<%1>) -> ()
// VERIFY:     }) : () -> !tlam.fun<!value<%1>, !value<%1>>
// VERIFY:     "tlam.treturn"(%2) : (!tlam.fun<!value<%1>, !value<%1>>) -> ()
// VERIFY:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// VERIFY: }

// -----

// Invalid: forward reference in result type.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %v = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !value<%T>
  %T = "test.make_type"() : () -> !tlam.type
}

// VERIFY: ssa-dominance: value Value{{.*}} does not dominate its use in op `builtin.unrealized_conversion_cast`

// -----

// Valid: dominating type value referenced from a nested attribute parameter.
builtin.module {
  %T = "test.make_type"() : () -> !tlam.type
  "test.use"() {dep = !tlam.forall<!tlam.fun<i32, !value<%T>>>} : () -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.make_type"() : () -> !tlam.type
// VERIFY:   "test.use"() {dep = !tlam.forall<!tlam.fun<i32, !value<%0>>>} : () -> ()
// VERIFY: }

// -----

// Invalid: forward reference in attribute payload.
builtin.module {
  "test.use"() {dep = !tlam.forall<!tlam.fun<i32, !value<%T>>>} : () -> ()
  %T = "test.make_type"() : () -> !tlam.type
}

// VERIFY: ssa-dominance: value Value{{.*}} does not dominate its use in op `test.use`

// -----

// Valid: AttributeWalker reaches tvars inside nested attribute lists.
builtin.module {
  %T = "test.make_type"() : () -> !tlam.type
  "test.use"() {nested = [!value<%T>, [!tlam.forall<!value<%T>>, !value<%T>]]} : () -> ()
}

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.make_type"() : () -> !tlam.type
// VERIFY:   "test.use"() {nested = [!value<%0>, [!tlam.forall<!value<%0>>, !value<%0>]]} : () -> ()
// VERIFY: }

// -----

// Invalid: forward reference in nested attribute list.
builtin.module {
  // expected-error @below {{ssa-dominance: value Value(!tlam.type) does not dominate its use in op `test.use`}}
  "test.use"() {nested = [!value<%T>, [!tlam.forall<!value<%T>>, !value<%T>]]} : () -> ()
  %T = "test.make_type"() : () -> !tlam.type
}

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

// VERIFY: builtin.module {
// VERIFY:   %0 = "test.make_type"() : () -> !tlam.type
// VERIFY:   %1 = "tlam.tlambda"() ({
// VERIFY:   ^bb0(%2: !tlam.type):
// VERIFY:     %3 = "test.make_i64"() : () -> i64
// VERIFY:     "tlam.treturn"(%3) : (i64) -> ()
// VERIFY:   }) : () -> !tlam.forall<i64>
// VERIFY:   %2 = "tlam.tapply"(%1) <{tyArg = !value<%0>}> : (!tlam.forall<i64>) -> i64
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

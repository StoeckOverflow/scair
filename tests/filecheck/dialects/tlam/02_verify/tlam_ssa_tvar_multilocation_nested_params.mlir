// Purpose: Deep Tvar embedding coverage across multiple IR locations.
// Invariants covered: embedded SSA-in-types references are rewritten consistently in result types, attrs, properties, and nested attribute lists.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=RAUW

// Valid: beta-reduction substitutes %x -> %A and rewrites all embedded tvar<%x> locations.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.type):
    // 1) Result type embedding.
    // 2) Attribute parameter embedding + 4) nested list parameter embedding.
    %tv = "builtin.unrealized_conversion_cast"(%x) {
      dep = !tlam.forall<!tlam.fun<!tlam.tvar<%x>, !tlam.forall<!tlam.tvar<%x>>>>,
      nested = [!tlam.tvar<%x>, [!tlam.forall<!tlam.tvar<%x>>, !tlam.tvar<%x>]]
    } : (!tlam.type) -> !tlam.tvar<%x>

    // 3) Property parameter embedding.
    %G = "tlam.tlambda"() ({
    ^bb1(%U: !tlam.type):
      %u = "builtin.unrealized_conversion_cast"(%U) : (!tlam.type) -> !tlam.tvar<%U>
      "tlam.treturn"(%u) : (!tlam.tvar<%U>) -> ()
    }) : () -> !tlam.forall<!tlam.bvar<0>>
    %h = "tlam.tapply"(%G) <{tyArg = !tlam.tvar<%x>}>
         : (!tlam.forall<!tlam.bvar<0>>) -> !tlam.tvar<%x>

    %back = "builtin.unrealized_conversion_cast"(%h) : (!tlam.tvar<%x>) -> !tlam.type
    "tlam.vreturn"(%back) : (!tlam.type) -> ()
  }) : () -> !tlam.fun<!tlam.type, !tlam.type>

  %A = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
  %r = "tlam.vapply"(%f, %A) : (!tlam.fun<!tlam.type, !tlam.type>, !tlam.type) -> !tlam.type
  %out = "builtin.unrealized_conversion_cast"(%r) : (!tlam.type) -> !tlam.type
}

// RAUW-LABEL: builtin.module {
// RAUW: [[A:%[0-9]+]] = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
// RAUW-NOT: "tlam.vapply"
// RAUW: [[TV:%[0-9]+]] = "builtin.unrealized_conversion_cast"([[A]]) {dep = !tlam.forall<!tlam.fun<!tlam.tvar<[[A]]>, !tlam.forall<!tlam.tvar<[[A]]>>>>, nested = [!tlam.tvar<[[TV]]>, [!tlam.forall<!tlam.tvar<[[TV]]>>, !tlam.tvar<[[TV]]>]]} : (!tlam.type) -> !tlam.tvar<[[A]]>
// RAUW: "tlam.tapply"(%{{[0-9]+}}) <{tyArg = !tlam.tvar<%{{[0-9]+}}>}> : (!tlam.forall<!tlam.bvar<0>>) -> !tlam.tvar<%{{[0-9]+}}>
// RAUW: "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (!tlam.tvar<[[A]]>) -> !tlam.type

// -----

// Invalid nested attr forward reference in embedded tvar.
// expected-error @below {{ssa-dominance: value Value}}
builtin.module {
  "test.use"() {nested = [!tlam.tvar<%T>, [!tlam.forall<!tlam.tvar<%T>>, !tlam.tvar<%T>]]} : () -> ()
  %T = "test.make_type"() : () -> !tlam.type
}

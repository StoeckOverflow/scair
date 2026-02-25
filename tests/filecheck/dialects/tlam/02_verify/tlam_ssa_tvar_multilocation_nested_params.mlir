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
      dep = !tlam.forall<!tlam.fun<!value<%x>, !tlam.forall<!value<%x>>>>,
      nested = [!value<%x>, [!tlam.forall<!value<%x>>, !value<%x>]]
    } : (!tlam.type) -> !value<%x>

    // 3) Property parameter embedding.
    %G = "tlam.tlambda"() ({
    ^bb1(%U: !tlam.type):
      %u = "builtin.unrealized_conversion_cast"(%U) : (!tlam.type) -> !value<%U>
      "tlam.treturn"(%u) : (!value<%U>) -> ()
    }) : () -> !tlam.forall<!tlam.bvar<0>>
    %h = "tlam.tapply"(%G) <{tyArg = !value<%x>}>
         : (!tlam.forall<!tlam.bvar<0>>) -> !value<%x>

    %back = "builtin.unrealized_conversion_cast"(%h) : (!value<%x>) -> !tlam.type
    "tlam.vreturn"(%back) : (!tlam.type) -> ()
  }) : () -> !tlam.fun<!tlam.type, !tlam.type>

  %A = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
  %r = "tlam.vapply"(%f, %A) : (!tlam.fun<!tlam.type, !tlam.type>, !tlam.type) -> !tlam.type
  %out = "builtin.unrealized_conversion_cast"(%r) : (!tlam.type) -> !tlam.type
}

// RAUW-LABEL: builtin.module {
// RAUW: [[A:%[0-9]+]] = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
// RAUW-NOT: "tlam.vapply"
// RAUW: [[TV:%[0-9]+]] = "builtin.unrealized_conversion_cast"([[A]]) {dep = !tlam.forall<!tlam.fun<!value<[[A]]>, !tlam.forall<!value<[[A]]>>>>, nested = [!value<[[TV]]>, [!tlam.forall<!value<[[TV]]>>, !value<[[TV]]>]]} : (!tlam.type) -> !value<[[A]]>
// RAUW: "tlam.tapply"(%{{[0-9]+}}) <{tyArg = !value<%{{[0-9]+}}>}> : (!tlam.forall<!tlam.bvar<0>>) -> !value<%{{[0-9]+}}>
// RAUW: "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (!value<[[A]]>) -> !tlam.type

// -----

// Invalid nested attr forward reference in embedded tvar.
// expected-error @below {{ssa-dominance: value Value}}
builtin.module {
  "test.use"() {nested = [!value<%T>, [!tlam.forall<!value<%T>>, !value<%T>]]} : () -> ()
  %T = "test.make_type"() : () -> !tlam.type
}

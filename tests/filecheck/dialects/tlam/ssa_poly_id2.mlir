// RUN: scair-opt -p=verify-type-params %s | filecheck %s --dump-input=fail --dump-input-filter all

builtin.module {
  // F = ΛT.(define G = ΛU. λ(x:U).x; h := G T; return h)
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    // G = ΛU. λ(x:U).x
    %G = "tlam.tlambda"() ({
    ^bb0(%U: !tlam.type):
      %v = "tlam.vlambda"() <{funAttr = !tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>}> ({
      ^bb0(%x: !tlam.tvar<%U>):
        "tlam.vreturn"(%x) <{expected = !tlam.tvar<%U>}> : (!tlam.tvar<%U>) -> ()
      }) : () -> (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>)

      "tlam.treturn"(%v)
        <{expected = !tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>}>
        : (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    // h = G T : T → T
    %h = "tlam.tapply"(%G)
      <{argType = !tlam.tvar<%T>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
        -> (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>)

    // return h : T → T
    "tlam.treturn"(%h)
      <{expected = !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>}>
      : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:   ^bb0(%1: !tlam.type):
// CHECK:     %2 = "tlam.tlambda"() ({
// CHECK:     ^bb1(%3: !tlam.type):
// CHECK:       %4 = "tlam.vlambda"() <{funAttr = !tlam.fun<!tlam.tvar<%3>, !tlam.tvar<%3>>}> ({
// CHECK:       ^bb2(%5: !tlam.tvar<%3>):
// CHECK:         "tlam.vreturn"(%5) <{expected = !tlam.tvar<%3>}> {expected = !tlam.tvar<%3>} : (!tlam.tvar<%3>) -> ()
// CHECK:       }) {funAttr = !tlam.fun<!tlam.tvar<%3>, !tlam.tvar<%3>>} : () -> !tlam.fun<!tlam.tvar<%3>, !tlam.tvar<%3>>
// CHECK:       "tlam.treturn"(%4) <{expected = !tlam.fun<!tlam.tvar<%3>, !tlam.tvar<%3>>}> {expected = !tlam.fun<!tlam.tvar<%3>, !tlam.tvar<%3>>} : (!tlam.fun<!tlam.tvar<%3>, !tlam.tvar<%3>>) -> ()
// CHECK:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK:     %3 = "tlam.tapply"(%2) <{argType = !tlam.tvar<%1>}> {argType = !tlam.tvar<%1>} : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>
// CHECK:     "tlam.treturn"(%3) <{expected = !tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>}> {expected = !tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>} : (!tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK: }

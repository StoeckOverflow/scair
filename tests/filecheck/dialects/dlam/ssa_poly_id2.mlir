// RUN: scair-opt -p=verify-dependent-types %s | filecheck %s --dump-input=fail --dump-input-filter all

builtin.module {
  // F = ΛT.(define G = ΛU. λ(x:U).x; h := G T; return h)
  %F = "dlam.tlambda"() ({
  ^bb0(%T: !dlam.type):
    // G = ΛU. λ(x:U).x
    %G = "dlam.tlambda"() ({
    ^bb0(%U: !dlam.type):
      %v = "dlam.vlambda"() <{funAttr = !dlam.fun<!dlam.tvar<%U>, !dlam.tvar<%U>>}> ({
      ^bb0(%x: !dlam.tvar<%U>):
        "dlam.vreturn"(%x) <{expected = !dlam.tvar<%U>}> : (!dlam.tvar<%U>) -> ()
      }) : () -> (!dlam.fun<!dlam.tvar<%U>, !dlam.tvar<%U>>)

      "dlam.treturn"(%v)
        <{expected = !dlam.fun<!dlam.tvar<%U>, !dlam.tvar<%U>>}>
        : (!dlam.fun<!dlam.tvar<%U>, !dlam.tvar<%U>>) -> ()
    }) : () -> (!dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>)

    // h = G T : T → T
    %h = "dlam.tapply"(%G)
      <{argType = !dlam.tvar<%T>}>
      : (!dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>)
        -> (!dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>)

    // return h : T → T
    "dlam.treturn"(%h)
      <{expected = !dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>}>
      : (!dlam.fun<!dlam.tvar<%T>, !dlam.tvar<%T>>) -> ()
  }) : () -> (!dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>)
}

// CHECK: builtin.module {
// CHECK:   %0 = "dlam.tlambda"() ({
// CHECK:   ^bb0(%1: !dlam.type):
// CHECK:     %2 = "dlam.tlambda"() ({
// CHECK:     ^bb1(%3: !dlam.type):
// CHECK:       %4 = "dlam.vlambda"() <{funAttr = !dlam.fun<!dlam.tvar<%3>, !dlam.tvar<%3>>}> ({
// CHECK:       ^bb2(%5: !dlam.tvar<%3>):
// CHECK:         "dlam.vreturn"(%5) <{expected = !dlam.tvar<%3>}> : (!dlam.tvar<%3>) -> ()
// CHECK:       }) : () -> !dlam.fun<!dlam.tvar<%3>, !dlam.tvar<%3>>
// CHECK:       "dlam.treturn"(%4) <{expected = !dlam.fun<!dlam.tvar<%3>, !dlam.tvar<%3>>}> : (!dlam.fun<!dlam.tvar<%3>, !dlam.tvar<%3>>) -> ()
// CHECK:     }) : () -> !dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>
// CHECK:     %3 = "dlam.tapply"(%2) <{argType = !dlam.tvar<%1>}> : (!dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>) -> !dlam.fun<!dlam.tvar<%1>, !dlam.tvar<%1>>
// CHECK:     "dlam.treturn"(%3) <{expected = !dlam.fun<!dlam.tvar<%1>, !dlam.tvar<%1>>}> : (!dlam.fun<!dlam.tvar<%1>, !dlam.tvar<%1>>) -> ()
// CHECK:   }) : () -> !dlam.forall<!dlam.fun<!dlam.bvar<0>, !dlam.bvar<0>>>
// CHECK: }
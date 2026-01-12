// RUN: scair-opt -p=verify-type-params %s | filecheck %s --dump-input=fail --dump-input-filter all

builtin.module {
  // F : ΛT. λ(x:T). x
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "tlam.vlambda"() <{funAttr = !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>}> ({
    ^bb0(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x) <{expected = !tlam.tvar<%T>}> : (!tlam.tvar<%T>) -> ()
    }) : () -> (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>)
    "tlam.treturn"(%v)
      <{expected = !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>}>
      : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:   ^bb0(%1: !tlam.type):
// CHECK:     %2 = "tlam.vlambda"() <{funAttr = !tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>}> ({
// CHECK:     ^bb1(%3: !tlam.tvar<%1>):
// CHECK:       "tlam.vreturn"(%3) <{expected = !tlam.tvar<%1>}> : (!tlam.tvar<%1>) -> ()
// CHECK:     }) : () -> !tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>
// CHECK:     "tlam.treturn"(%2) <{expected = !tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>}> : (!tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK: }
// RUN: scair-opt %s | filecheck %s --dump-input=fail --dump-input-filter all

builtin.module {
  // F = ΛT.(define G = ΛU. λ(x:U).x; h := G T; return h)
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    // G = ΛU. λ(x:U).x
    %G = "tlam.tlambda"() ({
    ^bb0(%U: !tlam.type):
      %v = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.tvar<%U>):
        "tlam.vreturn"(%x) : (!tlam.tvar<%U>) -> ()
      }) : () -> (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>)

      "tlam.treturn"(%v)
        : (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    // h = G T : T → T
    %h = "tlam.tapply"(%G) <{tyArg = !tlam.tvar<%T>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
        -> (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>)

    // return h : T → T
    "tlam.treturn"(%h)
      : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
}

// CHECK: builtin.module { 
// CHECK:   %0 = "tlam.tlambda"() ({ 
// CHECK:   ^bb0(%1: !tlam.type): 
// CHECK:     %2 = "tlam.tlambda"() ({ 
// CHECK:     ^bb1(%3: !tlam.type): 
// CHECK:       %4 = "tlam.vlambda"() ({ 
// CHECK:       ^bb2(%5: !tlam.tvar<%3>): 
// CHECK:         "tlam.vreturn"(%5) : (!tlam.tvar<%3>) -> () 
// CHECK:       }) : () -> !tlam.fun<!tlam.tvar<%3>, !tlam.tvar<%3>> 
// CHECK:       "tlam.treturn"(%4) : (!tlam.fun<!tlam.tvar<%3>, !tlam.tvar<%3>>) -> () 
// CHECK:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK:     %3 = "tlam.tapply"(%2) <{tyArg = !tlam.tvar<%1>}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>> 
// CHECK:     "tlam.treturn"(%3) : (!tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>) -> () 
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>> 
// CHECK: } 

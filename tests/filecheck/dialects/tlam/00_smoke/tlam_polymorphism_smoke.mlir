// Purpose: Smoke-verify parse/print of core TLam polymorphism forms.
// Invariants covered: Valid TLambda/Vlambda and nested TLambda/Tapply printing stability.

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// F : ΛT. λ(x:T). x
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

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:   ^bb0(%1: !tlam.type):
// CHECK:     %2 = "tlam.vlambda"() ({
// CHECK:     ^bb1(%3: !value<%1>):
// CHECK:       "tlam.vreturn"(%3) : (!value<%1>) -> ()
// CHECK:     }) : () -> !tlam.fun<!value<%1>, !value<%1>>
// CHECK:     "tlam.treturn"(%2) : (!tlam.fun<!value<%1>, !value<%1>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK: }

// -----

builtin.module {
  // F = ΛT.(define G = ΛU. λ(x:U).x; h := G T; return h)
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    // G = ΛU. λ(x:U).x
    %G = "tlam.tlambda"() ({
    ^bb0(%U: !tlam.type):
      %v = "tlam.vlambda"() ({
      ^bb0(%x: !value<%U>):
        "tlam.vreturn"(%x) : (!value<%U>) -> ()
      }) : () -> (!tlam.fun<!value<%U>, !value<%U>>)

      "tlam.treturn"(%v)
        : (!tlam.fun<!value<%U>, !value<%U>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    // h = G T : T → T
    %h = "tlam.tapply"(%G) <{tyArg = !value<%T>}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
        -> (!tlam.fun<!value<%T>, !value<%T>>)

    // return h : T → T
    "tlam.treturn"(%h)
      : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
}

// CHECK: builtin.module { 
// CHECK:   %0 = "tlam.tlambda"() ({ 
// CHECK:   ^bb0(%1: !tlam.type): 
// CHECK:     %2 = "tlam.tlambda"() ({ 
// CHECK:     ^bb1(%3: !tlam.type): 
// CHECK:       %4 = "tlam.vlambda"() ({ 
// CHECK:       ^bb2(%5: !value<%3>): 
// CHECK:         "tlam.vreturn"(%5) : (!value<%3>) -> () 
// CHECK:       }) : () -> !tlam.fun<!value<%3>, !value<%3>> 
// CHECK:       "tlam.treturn"(%4) : (!tlam.fun<!value<%3>, !value<%3>>) -> () 
// CHECK:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK:     %3 = "tlam.tapply"(%2) <{tyArg = !value<%1>}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<!value<%1>, !value<%1>> 
// CHECK:     "tlam.treturn"(%3) : (!tlam.fun<!value<%1>, !value<%1>>) -> () 
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>> 
// CHECK: } 

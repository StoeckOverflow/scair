// RUN: scair-opt %s --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// F : ΛT. λ(x:T). x
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%T>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>
    "tlam.treturn"(%v)
      : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:   ^bb0(%1: !tlam.type):
// CHECK:     %2 = "tlam.vlambda"() ({
// CHECK:     ^bb1(%3: !tlam.tvar<%1>):
// CHECK:       "tlam.vreturn"(%3) : (!tlam.tvar<%1>) -> ()
// CHECK:     }) : () -> !tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>
// CHECK:     "tlam.treturn"(%2) : (!tlam.fun<!tlam.tvar<%1>, !tlam.tvar<%1>>) -> ()
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

// -----

// Invalid tlambda binder type.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: i32):
    %v = "tlam.vlambda"() ({
    ^bb0(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> !tlam.fun<i32, i32>
    "tlam.treturn"(%v) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
}

// CHECK: tlambda: binder block argument must have type !tlam.type, got i32

// -----

// Invalid tlambda body (missing treturn).
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%T>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
}

// CHECK: tlambda: last op must be tlam.treturn, got 'tlam.vlambda'

// -----

// Invalid tapply result type.
builtin.module {
  %G = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %v = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.tvar<%U>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%U>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>
    "tlam.treturn"(%v)
      : (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %h = "tlam.tapply"(%G) <{tyArg = i64}>
       : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
         -> !tlam.fun<i32, i32>
}

// CHECK: tapply: result
// CHECK: instantiated

// -----

// Invalid vapply argument type.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 0 : i64}> : () -> i64
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i64) -> i32
}

// CHECK: vapply: expected arg i32 and result i32, got i64 and i32

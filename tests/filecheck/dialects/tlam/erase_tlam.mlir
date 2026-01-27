// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize,erase-tlam | filecheck %s

builtin.module {
  %0 = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %poly_id = "tlam.tlambda"() ({
    ^bb1(%U: !tlam.type):
      %id = "tlam.vlambda"() ({
      ^bb2(%x: !tlam.tvar<%U>):
        "tlam.vreturn"(%x): (!tlam.tvar<%U>) -> ()
      }) : () -> (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    %spec = "tlam.tapply"(%poly_id) <{tyArg = i64}>
           : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%spec) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}

// CHECK:       builtin.module {
// CHECK-NOT:     "tlam.tlambda"
// CHECK-NOT:     "tlam.treturn"
// CHECK-NEXT:    %0 = "tlam.vlambda"() ({
// CHECK-NEXT:    ^bb0(%1: i64): 
// CHECK-NEXT:      "tlam.vreturn"(%1) : (i64) -> () 
// CHECK-NEXT:    }) : () -> !tlam.fun<i64, i64>
// CHECK-NEXT:  }

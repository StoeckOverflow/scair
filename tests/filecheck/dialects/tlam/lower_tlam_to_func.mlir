// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize,erase-tlam,lower-tlam-to-func | filecheck %s

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
// CHECK-NOT:     "tlam."
// CHECK:         func.func @lifted_1(%0: i64) -> i64 {
// CHECK-NEXT:      func.return %0 : i64 
// CHECK-NEXT:    }
// CHECK:         %0 = func.constant @lifted_1 : (i64) -> i64
// CHECK:       }

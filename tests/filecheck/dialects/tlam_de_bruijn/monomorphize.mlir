// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize | filecheck %s

builtin.module {
  %0 = "tlam.tlambda"() ({
    %poly_id = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x): (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    %spec = "tlam.tapply"(%poly_id) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%spec) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}
// CHECK:       builtin.module {
// CHECK-NOT:     "tlam.tapply"
// CHECK-NEXT:    %0 = "tlam.tlambda"() ({
// CHECK-NEXT:      %1 = "tlam.vlambda"() ({
// CHECK-NEXT:      ^bb0(%2: i64):
// CHECK-NEXT:        "tlam.vreturn"(%2) : (i64) -> () 
// CHECK-NEXT:      }) : () -> !tlam.fun<i64, i64>
// CHECK-NEXT:      "tlam.treturn"(%1) : (!tlam.fun<i64, i64>) -> ()
// CHECK-NEXT:    }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// CHECK-NEXT:  }

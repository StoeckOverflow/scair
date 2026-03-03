// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize,erase-tlam | filecheck %s

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

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i64):
// CHECK:     "tlam.vreturn"(%1) : (i64) -> ()
// CHECK:   }) : () -> !tlam.fun<i64, i64>
// CHECK: }

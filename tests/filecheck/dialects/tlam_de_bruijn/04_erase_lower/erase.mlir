// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn | filecheck %s

builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
    %poly_id = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x): (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

    %spec = "tlam_dbi.tapply"(%poly_id) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    "tlam_dbi.treturn"(%spec) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
  %top = "tlam_dbi.tapply"(%0) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>) -> (!tlam_dbi.fun<i64, i64>)
  "test.use"(%top) : (!tlam_dbi.fun<i64, i64>) -> ()
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i64):
// CHECK:     "tlam_dbi.vreturn"(%1) : (i64) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i64, i64>
// CHECK: }

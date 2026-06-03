// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize-tlam-de-bruijn | filecheck %s

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
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     %1 = "tlam_dbi.tlambda"() ({
// CHECK:       %2 = "tlam_dbi.vlambda"() ({
// CHECK:       ^bb0(%3: !tlam_dbi.bvar<0>):
// CHECK:         "tlam_dbi.vreturn"(%3) : (!tlam_dbi.bvar<0>) -> ()
// CHECK:       }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// CHECK:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// CHECK:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// CHECK:     %2 = "tlam_dbi.vlambda"() ({
// CHECK:     ^bb0(%3: i64):
// CHECK:       "tlam_dbi.vreturn"(%3) : (i64) -> ()
// CHECK:     }) : () -> !tlam_dbi.fun<i64, i64>
// CHECK:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// CHECK: }

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p erase-tlam-de-bruijn | filecheck %s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics -p erase-tlam-de-bruijn | scair-opt --allow-unregistered-dialect --verify-diagnostics

// Regression: erase-tlam must not erase a binder that would leave free DBI types behind.
builtin.module {
  %poly = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
  "test.use"(%poly) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> ()
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     %1 = "tlam_dbi.vlambda"() ({
// CHECK:     ^bb0(%2: !tlam_dbi.bvar<0>):
// CHECK:       "tlam_dbi.vreturn"(%2) : (!tlam_dbi.bvar<0>) -> ()
// CHECK:     }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// CHECK:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// CHECK:   "test.use"(%0) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> ()
// CHECK: }

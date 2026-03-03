// RUN: scair-opt %s --allow-unregistered-dialect -p erase-tlam | filecheck %s

// Regression: erase-tlam must not erase a binder that would leave free DBI types behind.
builtin.module {
  %poly = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
  "test.use"(%poly) : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> ()
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:     %1 = "tlam.vlambda"() ({
// CHECK:     ^bb0(%2: !tlam.bvar<0>):
// CHECK:       "tlam.vreturn"(%2) : (!tlam.bvar<0>) -> ()
// CHECK:     }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// CHECK:     "tlam.treturn"(%1) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK:   "test.use"(%0) : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> ()
// CHECK: }

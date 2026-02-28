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

// CHECK: "tlam.tlambda"
// CHECK: !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

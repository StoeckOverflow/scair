// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// VALID: depth=3 allows bvar<2>.
builtin.module {
  %a = "tlam_dbi.tlambda"() ({
    %b = "tlam_dbi.tlambda"() ({
      %c = "tlam_dbi.tlambda"() ({
        %id = "tlam_dbi.vlambda"() ({
        ^bb0(%x: !tlam_dbi.bvar<2>):
          "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<2>) -> ()
        }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>)
        "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>) -> ()
      }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>)
      "tlam_dbi.treturn"(%c) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>>)
    "tlam_dbi.treturn"(%b) : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>>>)
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     %1 = "tlam_dbi.tlambda"() ({
// CHECK:       %2 = "tlam_dbi.tlambda"() ({
// CHECK:         %3 = "tlam_dbi.vlambda"() ({
// CHECK:         ^bb0(%4: !tlam_dbi.bvar<2>):
// CHECK:           "tlam_dbi.vreturn"(%4) : (!tlam_dbi.bvar<2>) -> ()
// CHECK:         }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>
// CHECK:         "tlam_dbi.treturn"(%3) : (!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>) -> ()
// CHECK:       }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>
// CHECK:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>) -> ()
// CHECK:     }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>>
// CHECK:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<2>, !tlam_dbi.bvar<2>>>>>
// CHECK: }

// -----

// INVALID: depth=3 rejects bvar<3>.
builtin.module {
  %a = "tlam_dbi.tlambda"() ({
    %b = "tlam_dbi.tlambda"() ({
      %c = "tlam_dbi.tlambda"() ({
        %id = "tlam_dbi.vlambda"() ({
        ^bb0(%x: !tlam_dbi.bvar<3>):
          "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<3>) -> ()
        }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>)
        "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>) -> ()
      }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>>)
      "tlam_dbi.treturn"(%c) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>>>)
    "tlam_dbi.treturn"(%b) : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<3>, !tlam_dbi.bvar<3>>>>>)
}
// CHECK: debruijn-dbi: bvar<3> out of scope at depth=3

// -----

// INVALID: vlambda with multiple blocks is rejected.
builtin.module {
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  ^bb1(%y: i32):
    "tlam_dbi.vreturn"(%y) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
}
// CHECK: vlambda: one block with one arg of input type required

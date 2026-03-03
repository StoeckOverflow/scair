// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// VALID: depth=3 allows bvar<2>.
builtin.module {
  %a = "tlam.tlambda"() ({
    %b = "tlam.tlambda"() ({
      %c = "tlam.tlambda"() ({
        %id = "tlam.vlambda"() ({
        ^bb0(%x: !tlam.bvar<2>):
          "tlam.vreturn"(%x) : (!tlam.bvar<2>) -> ()
        }) : () -> (!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>)
        "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>) -> ()
      }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>)
      "tlam.treturn"(%c) : (!tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>) -> ()
    }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>>)
    "tlam.treturn"(%b) : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>>) -> ()
  }) : () -> (!tlam.forall<!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>>>)
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:     %1 = "tlam.tlambda"() ({
// CHECK:       %2 = "tlam.tlambda"() ({
// CHECK:         %3 = "tlam.vlambda"() ({
// CHECK:         ^bb0(%4: !tlam.bvar<2>):
// CHECK:           "tlam.vreturn"(%4) : (!tlam.bvar<2>) -> ()
// CHECK:         }) : () -> !tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>
// CHECK:         "tlam.treturn"(%3) : (!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>) -> ()
// CHECK:       }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>
// CHECK:       "tlam.treturn"(%2) : (!tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>) -> ()
// CHECK:     }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>>
// CHECK:     "tlam.treturn"(%1) : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<2>, !tlam.bvar<2>>>>>
// CHECK: }

// -----

// INVALID: depth=3 rejects bvar<3>.
builtin.module {
  %a = "tlam.tlambda"() ({
    %b = "tlam.tlambda"() ({
      %c = "tlam.tlambda"() ({
        %id = "tlam.vlambda"() ({
        ^bb0(%x: !tlam.bvar<3>):
          "tlam.vreturn"(%x) : (!tlam.bvar<3>) -> ()
        }) : () -> (!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>)
        "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>) -> ()
      }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>>)
      "tlam.treturn"(%c) : (!tlam.forall<!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>>) -> ()
    }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>>>)
    "tlam.treturn"(%b) : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>>>) -> ()
  }) : () -> (!tlam.forall<!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<3>, !tlam.bvar<3>>>>>)
}
// CHECK: debruijn: bvar<3> out of scope at depth=3

// -----

// INVALID: vlambda with multiple blocks is rejected.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  ^bb1(%y: i32):
    "tlam.vreturn"(%y) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
}
// CHECK: vlambda: one block with one arg of input type required

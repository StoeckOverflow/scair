// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize-tlam-de-bruijn | filecheck %s

builtin.module {
  %0 = "tlam_dbi.tlambda"() ({
    %g = "tlam_dbi.tlambda"() ({
      %h = "tlam_dbi.tlambda"() ({
        %u = "test.op"() : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>)
        "tlam_dbi.treturn"(%u) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>) -> ()
      }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>)
      "tlam_dbi.treturn"(%h) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>)
    %spec = "tlam_dbi.tapply"(%g) <{tyArg = !tlam_dbi.bvar<0>}> : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>) -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>)
    "tlam_dbi.treturn"(%spec) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>)
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     %1 = "tlam_dbi.tlambda"() ({
// CHECK:       %2 = "test.op"() : () -> !tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>
// CHECK:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>) -> ()
// CHECK:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// CHECK:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// CHECK: }

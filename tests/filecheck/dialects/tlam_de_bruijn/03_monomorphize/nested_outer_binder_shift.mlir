// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn | filecheck %s -DFILE=%s

// Regression: specialization under nested binders keeps bvar<1>/bvar<0> structure.
builtin.module {
  %f = "tlam_dbi.tlambda"() ({
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
// CHECK:       %2 = "tlam_dbi.tlambda"() ({
// CHECK:         %3 = "test.op"() : () -> !tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>
// CHECK:         "tlam_dbi.treturn"(%3) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>) -> ()
// CHECK:       }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// CHECK:       "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// CHECK:     }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// CHECK:     %2 = "tlam_dbi.tlambda"() ({
// CHECK:       %3 = "test.op"() : () -> !tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>
// CHECK:       "tlam_dbi.treturn"(%3) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>) -> ()
// CHECK:     }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// CHECK:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// CHECK: }

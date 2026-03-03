// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize | filecheck %s -DFILE=%s

// Regression: specialization under nested binders keeps bvar<1>/bvar<0> structure.
builtin.module {
  %f = "tlam.tlambda"() ({
    %g = "tlam.tlambda"() ({
      %h = "tlam.tlambda"() ({
        %u = "test.op"() : () -> (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>)
        "tlam.treturn"(%u) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
      }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>)
      "tlam.treturn"(%h) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
    }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>)
    %spec = "tlam.tapply"(%g) <{tyArg = !tlam.bvar<0>}> : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>) -> (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>)
    "tlam.treturn"(%spec) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
  }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>)
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:     %1 = "tlam.tlambda"() ({
// CHECK:       %2 = "test.op"() : () -> !tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>
// CHECK:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
// CHECK:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// CHECK:     "tlam.treturn"(%1) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>
// CHECK: }

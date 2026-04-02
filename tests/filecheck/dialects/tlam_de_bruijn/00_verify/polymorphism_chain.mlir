// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// VALID: nested explicit polymorphism with two consecutive tapply and value use.
builtin.module {
  %poly2 = "tlam_dbi.tlambda"() ({
    %inner = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
    "tlam_dbi.treturn"(%inner) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>>)

  %one = "tlam_dbi.tapply"(%poly2) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>>) -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
  %two = "tlam_dbi.tapply"(%one) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i32, i32>)
  %c1 = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %r = "tlam_dbi.vapply"(%two, %c1) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%r) : (i32) -> ()
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
// CHECK:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>>
// CHECK:   %1 = "tlam_dbi.tapply"(%0) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>>) -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>
// CHECK:   %2 = "tlam_dbi.tapply"(%1) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i32, i32>
// CHECK:   %3 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK:   %4 = "tlam_dbi.vapply"(%2, %3) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// CHECK:   "test.use"(%4) : (i32) -> ()
// CHECK: }

// -----

// VALID: capture-avoid instantiation through nested binders.
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
// CHECK:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = !tlam_dbi.bvar<0>}> : (!tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>) -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>
// CHECK:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<0>>>>
// CHECK: }

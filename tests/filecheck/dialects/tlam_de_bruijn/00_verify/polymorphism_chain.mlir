// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// VALID: nested explicit polymorphism with two consecutive tapply and value use.
builtin.module {
  %poly2 = "tlam.tlambda"() ({
    %inner = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
    "tlam.treturn"(%inner) : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> ()
  }) : () -> (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>>)

  %one = "tlam.tapply"(%poly2) <{tyArg = i32}> : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>>) -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
  %two = "tlam.tapply"(%one) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i32, i32>)
  %c1 = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %r = "tlam.vapply"(%two, %c1) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%r) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:     %1 = "tlam.tlambda"() ({
// CHECK:       %2 = "tlam.vlambda"() ({
// CHECK:       ^bb0(%3: !tlam.bvar<0>):
// CHECK:         "tlam.vreturn"(%3) : (!tlam.bvar<0>) -> ()
// CHECK:       }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
// CHECK:       "tlam.treturn"(%2) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
// CHECK:     }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK:     "tlam.treturn"(%1) : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>>
// CHECK:   %1 = "tlam.tapply"(%0) <{tyArg = i32}> : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>>) -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK:   %2 = "tlam.tapply"(%1) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
// CHECK:   %3 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK:   %4 = "tlam.vapply"(%2, %3) : (!tlam.fun<i32, i32>, i32) -> i32
// CHECK:   "test.use"(%4) : (i32) -> ()
// CHECK: }

// -----

// VALID: capture-avoid instantiation through nested binders.
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
// CHECK:       %2 = "tlam.tlambda"() ({
// CHECK:         %3 = "test.op"() : () -> !tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>
// CHECK:         "tlam.treturn"(%3) : (!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>) -> ()
// CHECK:       }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// CHECK:       "tlam.treturn"(%2) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
// CHECK:     }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>
// CHECK:     %2 = "tlam.tapply"(%1) <{tyArg = !tlam.bvar<0>}> : (!tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>) -> !tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>
// CHECK:     "tlam.treturn"(%2) : (!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.forall<!tlam.fun<!tlam.bvar<1>, !tlam.bvar<0>>>>
// CHECK: }

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics | filecheck %s -DFILE=%s

// Roundtrip-style stress: mixed value/type lambdas + applies must remain parse/print stable.
builtin.module {
  %outer = "tlam.tlambda"() ({
    %poly = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
    %spec = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    %c = "arith.constant"() <{value = 11 : i64}> : () -> (i64)
    %r = "tlam.vapply"(%spec, %c) : (!tlam.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()
    "tlam.treturn"(%spec) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
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
// CHECK:     %2 = "tlam.tapply"(%1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
// CHECK:     %3 = "arith.constant"() <{value = 11}> : () -> i64
// CHECK:     %4 = "tlam.vapply"(%2, %3) : (!tlam.fun<i64, i64>, i64) -> i64
// CHECK:     "test.use"(%4) : (i64) -> ()
// CHECK:     "tlam.treturn"(%2) : (!tlam.fun<i64, i64>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// CHECK: }

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file --verify-diagnostics | filecheck %s -DFILE=%s

// Roundtrip-style stress: mixed value/type lambdas + applies must remain parse/print stable.
builtin.module {
  %outer = "tlam_dbi.tlambda"() ({
    %poly = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
    %spec = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    %c = "arith.constant"() <{value = 11 : i64}> : () -> (i64)
    %r = "tlam_dbi.vapply"(%spec, %c) : (!tlam_dbi.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()
    "tlam_dbi.treturn"(%spec) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
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
// CHECK:     %2 = "tlam_dbi.tapply"(%1) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
// CHECK:     %3 = "arith.constant"() <{value = 11}> : () -> i64
// CHECK:     %4 = "tlam_dbi.vapply"(%2, %3) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
// CHECK:     "test.use"(%4) : (i64) -> ()
// CHECK:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// CHECK: }

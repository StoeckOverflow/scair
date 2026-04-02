// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn | filecheck %s -DFILE=%s

// Monomorphize with two different type arguments.
builtin.module {
  %outer = "tlam_dbi.tlambda"() ({
    %poly = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

    %a = "tlam_dbi.tapply"(%poly) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i32, i32>)
    %b = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    "test.use"(%a) : (!tlam_dbi.fun<i32, i32>) -> ()
    "tlam_dbi.treturn"(%b) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam_dbi.tlambda"() ({
// CHECK:     %1 = "tlam_dbi.vlambda"() ({
// CHECK:     ^bb0(%2: i32):
// CHECK:       "tlam_dbi.vreturn"(%2) : (i32) -> ()
// CHECK:     }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:     %2 = "tlam_dbi.vlambda"() ({
// CHECK:     ^bb0(%3: i64):
// CHECK:       "tlam_dbi.vreturn"(%3) : (i64) -> ()
// CHECK:     }) : () -> !tlam_dbi.fun<i64, i64>
// CHECK:     "test.use"(%1) : (!tlam_dbi.fun<i32, i32>) -> ()
// CHECK:     "tlam_dbi.treturn"(%2) : (!tlam_dbi.fun<i64, i64>) -> ()
// CHECK:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>
// CHECK: }

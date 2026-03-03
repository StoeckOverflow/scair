// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize | filecheck %s -DFILE=%s

// Monomorphize with two different type arguments.
builtin.module {
  %outer = "tlam.tlambda"() ({
    %poly = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    %a = "tlam.tapply"(%poly) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i32, i32>)
    %b = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    "test.use"(%a) : (!tlam.fun<i32, i32>) -> ()
    "tlam.treturn"(%b) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:     %1 = "tlam.vlambda"() ({
// CHECK:     ^bb0(%2: i32):
// CHECK:       "tlam.vreturn"(%2) : (i32) -> ()
// CHECK:     }) : () -> !tlam.fun<i32, i32>
// CHECK:     %2 = "tlam.vlambda"() ({
// CHECK:     ^bb0(%3: i64):
// CHECK:       "tlam.vreturn"(%3) : (i64) -> ()
// CHECK:     }) : () -> !tlam.fun<i64, i64>
// CHECK:     "test.use"(%1) : (!tlam.fun<i32, i32>) -> ()
// CHECK:     "tlam.treturn"(%2) : (!tlam.fun<i64, i64>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
// CHECK: }

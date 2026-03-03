// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize | filecheck %s -DFILE=%s

// Regression: top-level tapply should be rewritten (and not crash).
builtin.module {
  %poly = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

  %spec = "tlam.tapply"(%poly) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i32, i32>)
  "test.use"(%spec) : (!tlam.fun<i32, i32>) -> ()
}
// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     "tlam.vreturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   "test.use"(%0) : (!tlam.fun<i32, i32>) -> ()
// CHECK: }

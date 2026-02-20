// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam,lower-tlam-to-func | filecheck %s -DFILE=%s

// Lowering with two value lambdas and chained applies.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)

  %g = "tlam.vlambda"() ({
  ^bb0(%y: i32):
    %r = "tlam.vapply"(%f, %y) : (!tlam.fun<i32, i32>, i32) -> (i32)
    "tlam.vreturn"(%r) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)

  %c = "arith.constant"() <{value = 4 : i32}> : () -> (i32)
  %z = "tlam.vapply"(%g, %c) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%z) : (i32) -> ()
}

// CHECK-NOT: "tlam.vlambda"
// CHECK-NOT: "tlam.vapply"
// CHECK: func.func

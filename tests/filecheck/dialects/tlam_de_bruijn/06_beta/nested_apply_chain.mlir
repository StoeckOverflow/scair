// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam --verify-diagnostics | filecheck %s -DFILE=%s

// Beta-reduce nested direct applications.
builtin.module {
  "test.case.nested_chain"() : () -> ()
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %g = "tlam.vlambda"() ({
    ^bb0(%y: i32):
      "tlam.vreturn"(%y) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    %r = "tlam.vapply"(%g, %x) : (!tlam.fun<i32, i32>, i32) -> (i32)
    "tlam.vreturn"(%r) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %a = "arith.constant"() <{value = 9 : i32}> : () -> (i32)
  %z = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%z) : (i32) -> ()
}
// CHECK-LABEL: "test.case.nested_chain"() : () -> ()
// CHECK-NOT: "tlam.vapply"
// CHECK: "test.use"

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
// CHECK: builtin.module {
// CHECK:   "test.case.nested_chain"() : () -> ()
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "tlam.vlambda"() ({
// CHECK:     ^bb1(%3: i32):
// CHECK:       "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:     }) : () -> !tlam.fun<i32, i32>
// CHECK:     "tlam.vreturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 9 : i32}> : () -> i32
// CHECK:   %2 = "tlam.vlambda"() ({
// CHECK:   ^bb1(%3: i32):
// CHECK:     "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   "test.use"(%1) : (i32) -> ()
// CHECK: }

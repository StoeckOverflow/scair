// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam-de-bruijn --verify-diagnostics | filecheck %s -DFILE=%s

// Beta-reduce nested direct applications.
builtin.module {
  "test.case.nested_chain"() : () -> ()
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    %g = "tlam_dbi.vlambda"() ({
    ^bb0(%y: i32):
      "tlam_dbi.vreturn"(%y) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    %r = "tlam_dbi.vapply"(%g, %x) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
    "tlam_dbi.vreturn"(%r) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "arith.constant"() <{value = 9 : i32}> : () -> (i32)
  %z = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%z) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.nested_chain"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "tlam_dbi.vlambda"() ({
// CHECK:     ^bb1(%3: i32):
// CHECK:       "tlam_dbi.vreturn"(%3) : (i32) -> ()
// CHECK:     }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 9 : i32}> : () -> i32
// CHECK:   %2 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb1(%3: i32):
// CHECK:     "tlam_dbi.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   "test.use"(%1) : (i32) -> ()
// CHECK: }

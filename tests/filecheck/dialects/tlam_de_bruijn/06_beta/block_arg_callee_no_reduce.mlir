// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam-de-bruijn --verify-diagnostics | filecheck %s -DFILE=%s

// Must not reduce vapply when callee is a block argument (not direct vlambda producer).
builtin.module {
  "test.case.block_arg_callee"() : () -> ()
  %driver = "tlam_dbi.vlambda"() ({
  ^bb0(%f: !tlam_dbi.fun<i32, i32>):
    %a = "arith.constant"() <{value = 2 : i32}> : () -> (i32)
    %r = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
    "tlam_dbi.vreturn"(%r) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<!tlam_dbi.fun<i32, i32>, i32>)

  %id = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)

  %y = "tlam_dbi.vapply"(%driver, %id) : (!tlam_dbi.fun<!tlam_dbi.fun<i32, i32>, i32>, !tlam_dbi.fun<i32, i32>) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.block_arg_callee"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: !tlam_dbi.fun<i32, i32>):
// CHECK:     %2 = "arith.constant"() <{value = 2 : i32}> : () -> i32
// CHECK:     %3 = "tlam_dbi.vapply"(%1, %2) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// CHECK:     "tlam_dbi.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<!tlam_dbi.fun<i32, i32>, i32>
// CHECK:   %1 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%2: i32):
// CHECK:     "tlam_dbi.vreturn"(%2) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<!tlam_dbi.fun<i32, i32>, i32>, !tlam_dbi.fun<i32, i32>) -> i32
// CHECK:   "test.use"(%2) : (i32) -> ()
// CHECK: }

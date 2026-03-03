// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam --verify-diagnostics | filecheck %s -DFILE=%s

// Must not reduce vapply when callee is a block argument (not direct vlambda producer).
builtin.module {
  "test.case.block_arg_callee"() : () -> ()
  %driver = "tlam.vlambda"() ({
  ^bb0(%f: !tlam.fun<i32, i32>):
    %a = "arith.constant"() <{value = 2 : i32}> : () -> (i32)
    %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
    "tlam.vreturn"(%r) : (i32) -> ()
  }) : () -> (!tlam.fun<!tlam.fun<i32, i32>, i32>)

  %id = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)

  %y = "tlam.vapply"(%driver, %id) : (!tlam.fun<!tlam.fun<i32, i32>, i32>, !tlam.fun<i32, i32>) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.block_arg_callee"() : () -> ()
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: !tlam.fun<i32, i32>):
// CHECK:     %2 = "arith.constant"() <{value = 2 : i32}> : () -> i32
// CHECK:     %3 = "tlam.vapply"(%1, %2) : (!tlam.fun<i32, i32>, i32) -> i32
// CHECK:     "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<!tlam.fun<i32, i32>, i32>
// CHECK:   %1 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%2: i32):
// CHECK:     "tlam.vreturn"(%2) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<!tlam.fun<i32, i32>, i32>, !tlam.fun<i32, i32>) -> i32
// CHECK:   "test.use"(%2) : (i32) -> ()
// CHECK: }

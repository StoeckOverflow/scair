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
// CHECK-LABEL: "test.case.block_arg_callee"() : () -> ()
// CHECK: "tlam.vapply"(%{{.*}}, %{{.*}}) : (!tlam.fun<i32, i32>, i32) -> i32

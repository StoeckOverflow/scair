// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam --verify-diagnostics | filecheck %s -DFILE=%s

// VALID REDUCTION: identity lambda
builtin.module {
  "test.case.identity"() : () -> ()
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %a = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %y = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK-LABEL: "test.case.identity"() : () -> ()
// CHECK: %[[A:.*]] = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK: "test.use"(%[[A]]) : (i32) -> ()

// -----

// VALID REDUCTION: body is pure and gets cloned
builtin.module {
  "test.case.pure_clone"() : () -> ()
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %sum = "arith.addi"(%x, %x) : (i32, i32) -> i32
    "tlam.vreturn"(%sum) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %a = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %y = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK-LABEL: "test.case.pure_clone"() : () -> ()
// CHECK: %[[A:.*]] = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK: %[[SUM:.*]] = "arith.addi"(%[[A]], %[[A]]) : (i32, i32) -> i32
// CHECK: "test.use"(%[[SUM]]) : (i32) -> ()

// -----

// MUST NOT REDUCE: indirect callee (not directly produced by tlam.vlambda)
builtin.module {
  "test.case.indirect"() : () -> ()
  %fun = "test.op"() : () -> (!tlam.fun<i32, i32>)
  %a = "test.op"() : () -> (i32)
  %y = "tlam.vapply"(%fun, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK-LABEL: "test.case.indirect"() : () -> ()
// CHECK: "tlam.vapply"

// -----

// MUST NOT REDUCE: side-effecting op in lambda body (unknown op)
builtin.module {
  "test.case.effect_body"() : () -> ()
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %e = "test.effect"() : () -> (i32)
    "tlam.vreturn"(%e) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %a = "test.op"() : () -> (i32)
  %y = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK-LABEL: "test.case.effect_body"() : () -> ()
// CHECK: "tlam.vapply"

// -----

// MUST NOT REDUCE: effectful argument consumed more than once in body
builtin.module {
  "test.case.effect_arg_dup"() : () -> ()
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %sum = "arith.addi"(%x, %x) : (i32, i32) -> i32
    "tlam.vreturn"(%sum) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %a = "test.effect"() : () -> (i32)
  %y = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK-LABEL: "test.case.effect_arg_dup"() : () -> ()
// CHECK: "tlam.vapply"

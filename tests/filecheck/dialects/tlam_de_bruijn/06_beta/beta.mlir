// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam-de-bruijn --verify-diagnostics | filecheck %s -DFILE=%s

// VALID REDUCTION: identity lambda
builtin.module {
  "test.case.identity"() : () -> ()
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %y = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.identity"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK:   "test.use"(%1) : (i32) -> ()
// CHECK: }

// -----

// VALID REDUCTION: body is pure and gets cloned
builtin.module {
  "test.case.pure_clone"() : () -> ()
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    %sum = "arith.addi"(%x, %x) : (i32, i32) -> i32
    "tlam_dbi.vreturn"(%sum) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %y = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.pure_clone"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "arith.addi"(%1, %1) : (i32, i32) -> i32
// CHECK:     "tlam_dbi.vreturn"(%2) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK:   %2 = "arith.addi"(%1, %1) : (i32, i32) -> i32
// CHECK:   "test.use"(%2) : (i32) -> ()
// CHECK: }

// -----

// MUST NOT REDUCE: indirect callee (not directly produced by tlam.vlambda)
builtin.module {
  "test.case.indirect"() : () -> ()
  %fun = "test.op"() : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "test.op"() : () -> (i32)
  %y = "tlam_dbi.vapply"(%fun, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.indirect"() : () -> ()
// CHECK:   %0 = "test.op"() : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "test.op"() : () -> i32
// CHECK:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// CHECK:   "test.use"(%2) : (i32) -> ()
// CHECK: }

// -----

// MUST NOT REDUCE: side-effecting op in lambda body (unknown op)
builtin.module {
  "test.case.effect_body"() : () -> ()
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    %e = "test.effect"() : () -> (i32)
    "tlam_dbi.vreturn"(%e) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "test.op"() : () -> (i32)
  %y = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.effect_body"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "test.effect"() : () -> i32
// CHECK:     "tlam_dbi.vreturn"(%2) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "test.op"() : () -> i32
// CHECK:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// CHECK:   "test.use"(%2) : (i32) -> ()
// CHECK: }

// -----

// MUST NOT REDUCE: effectful argument consumed more than once in body
builtin.module {
  "test.case.effect_arg_dup"() : () -> ()
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    %sum = "arith.addi"(%x, %x) : (i32, i32) -> i32
    "tlam_dbi.vreturn"(%sum) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "test.effect"() : () -> (i32)
  %y = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.effect_arg_dup"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "arith.addi"(%1, %1) : (i32, i32) -> i32
// CHECK:     "tlam_dbi.vreturn"(%2) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "test.effect"() : () -> i32
// CHECK:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// CHECK:   "test.use"(%2) : (i32) -> ()
// CHECK: }

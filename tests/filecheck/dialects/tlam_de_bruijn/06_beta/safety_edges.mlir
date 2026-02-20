// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam --verify-diagnostics | filecheck %s

// VALID: effectful argument used once is still reducible with current conservative policy.
builtin.module {
  "test.case.effect_arg_single_use"() : () -> ()
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %a = "test.effect"() : () -> (i32)
  %y = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK-LABEL: "test.case.effect_arg_single_use"() : () -> ()
// CHECK-NOT: "tlam.vapply"
// CHECK: "test.use"

// -----

// MUST NOT REDUCE: malformed lambda body (missing tlam.vreturn) - rejected by verifier, no crash.
builtin.module {
  // expected-error @below {{vlambda: last op must be tlam.vreturn}}
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "test.op"() : () -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %a = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %y = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}

// -----

// MUST NOT REDUCE: direct callee with side-effecting body remains as vapply.
builtin.module {
  "test.case.effect_body_no_reduce"() : () -> ()
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %e = "test.effect"() : () -> (i32)
    "tlam.vreturn"(%e) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %a = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %y = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK-LABEL: "test.case.effect_body_no_reduce"() : () -> ()
// CHECK: "tlam.vapply"

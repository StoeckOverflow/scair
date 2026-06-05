// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam-de-bruijn --verify-diagnostics | filecheck %s

// VALID: effectful argument used once is still reducible with current conservative policy.
builtin.module {
  "test.case.effect_arg_single_use"() : () -> ()
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "tlam_dbi.vreturn"(%x) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "test.effect"() : () -> (i32)
  %y = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.effect_arg_single_use"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "test.effect"() : () -> i32
// CHECK:   "test.use"(%1) : (i32) -> ()
// CHECK: }

// -----

// MUST NOT REDUCE: malformed lambda body (missing tlam_dbi.vreturn) - rejected by verifier, no crash.
builtin.module {
  // expected-error @below {{vlambda: last op must be tlam_dbi.vreturn}}
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    "test.op"() : () -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
	  %y = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
	  "test.use"(%y) : (i32) -> ()
}

// CHECK: vlambda: last op must be tlam_dbi.vreturn

// -----

// MUST NOT REDUCE: direct callee with side-effecting body remains as vapply.
builtin.module {
  "test.case.effect_body_no_reduce"() : () -> ()
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    %e = "test.effect"() : () -> (i32)
    "tlam_dbi.vreturn"(%e) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "arith.constant"() <{value = 1 : i32}> : () -> (i32)
  %y = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%y) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.effect_body_no_reduce"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "test.effect"() : () -> i32
// CHECK:     "tlam_dbi.vreturn"(%2) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// CHECK:   "test.use"(%2) : (i32) -> ()
// CHECK: }

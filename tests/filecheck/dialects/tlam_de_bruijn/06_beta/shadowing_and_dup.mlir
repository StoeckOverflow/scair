// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam-de-bruijn --verify-diagnostics | filecheck %s -DFILE=%s

// VALID: beta-reduction with nested lambda shadowing remains well-formed.
builtin.module {
  "test.case.shadowing"() : () -> ()
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    %g = "tlam_dbi.vlambda"() ({
    ^bb0(%y: i32):
      "tlam_dbi.vreturn"(%y) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    %r = "tlam_dbi.vapply"(%g, %x) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
    "tlam_dbi.vreturn"(%r) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %a = "arith.constant"() <{value = 7 : i32}> : () -> (i32)
  %z = "tlam_dbi.vapply"(%f, %a) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%z) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.shadowing"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "tlam_dbi.vlambda"() ({
// CHECK:     ^bb1(%3: i32):
// CHECK:       "tlam_dbi.vreturn"(%3) : (i32) -> ()
// CHECK:     }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// CHECK:   %2 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb1(%3: i32):
// CHECK:     "tlam_dbi.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   "test.use"(%1) : (i32) -> ()
// CHECK: }

// -----

// MUST NOT REDUCE: effectful argument duplicated in body.
builtin.module {
  "test.case.effect_arg_dup_2"() : () -> ()
  %f = "tlam_dbi.vlambda"() ({
  ^bb0(%x: i32):
    %u1 = "arith.addi"(%x, %x) : (i32, i32) -> i32
    %u2 = "arith.addi"(%u1, %x) : (i32, i32) -> i32
    "tlam_dbi.vreturn"(%u2) : (i32) -> ()
  }) : () -> (!tlam_dbi.fun<i32, i32>)
  %eff = "test.effect"() : () -> (i32)
  %r = "tlam_dbi.vapply"(%f, %eff) : (!tlam_dbi.fun<i32, i32>, i32) -> (i32)
  "test.use"(%r) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.effect_arg_dup_2"() : () -> ()
// CHECK:   %0 = "tlam_dbi.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "arith.addi"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
// CHECK:     %3 = "arith.addi"(%2, %1) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
// CHECK:     "tlam_dbi.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam_dbi.fun<i32, i32>
// CHECK:   %1 = "test.effect"() : () -> i32
// CHECK:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
// CHECK:   "test.use"(%2) : (i32) -> ()
// CHECK: }

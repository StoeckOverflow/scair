// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam --verify-diagnostics | filecheck %s -DFILE=%s

// VALID: beta-reduction with nested lambda shadowing remains well-formed.
builtin.module {
  "test.case.shadowing"() : () -> ()
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %g = "tlam.vlambda"() ({
    ^bb0(%y: i32):
      "tlam.vreturn"(%y) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    %r = "tlam.vapply"(%g, %x) : (!tlam.fun<i32, i32>, i32) -> (i32)
    "tlam.vreturn"(%r) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %a = "arith.constant"() <{value = 7 : i32}> : () -> (i32)
  %z = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%z) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.shadowing"() : () -> ()
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "tlam.vlambda"() ({
// CHECK:     ^bb1(%3: i32):
// CHECK:       "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:     }) : () -> !tlam.fun<i32, i32>
// CHECK:     "tlam.vreturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// CHECK:   %2 = "tlam.vlambda"() ({
// CHECK:   ^bb1(%3: i32):
// CHECK:     "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   "test.use"(%1) : (i32) -> ()
// CHECK: }

// -----

// MUST NOT REDUCE: effectful argument duplicated in body.
builtin.module {
  "test.case.effect_arg_dup_2"() : () -> ()
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %u1 = "arith.addi"(%x, %x) : (i32, i32) -> i32
    %u2 = "arith.addi"(%u1, %x) : (i32, i32) -> i32
    "tlam.vreturn"(%u2) : (i32) -> ()
  }) : () -> (!tlam.fun<i32, i32>)
  %eff = "test.effect"() : () -> (i32)
  %r = "tlam.vapply"(%f, %eff) : (!tlam.fun<i32, i32>, i32) -> (i32)
  "test.use"(%r) : (i32) -> ()
}
// CHECK: builtin.module {
// CHECK:   "test.case.effect_arg_dup_2"() : () -> ()
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "arith.addi"(%1, %1) : (i32, i32) -> i32
// CHECK:     %3 = "arith.addi"(%2, %1) : (i32, i32) -> i32
// CHECK:     "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   %1 = "test.effect"() : () -> i32
// CHECK:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// CHECK:   "test.use"(%2) : (i32) -> ()
// CHECK: }

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
// CHECK-LABEL: "test.case.shadowing"() : () -> ()
// CHECK: "test.use"

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
// CHECK-LABEL: "test.case.effect_arg_dup_2"() : () -> ()
// CHECK: "tlam.vapply"

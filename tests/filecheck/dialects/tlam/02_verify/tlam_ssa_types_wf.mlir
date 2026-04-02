// Purpose: High-level WF smoke for TLam SSA-in-types.
// Invariants covered: end-to-end valid form plus apply typing checks.

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Targets: compact WF coverage. Detailed dominance/DBI/region checks live in
// dedicated files under 02_verify/.

// Note: wrong-type tvar parse-time failures are covered in
// tests/filecheck/dialects/tlam/invalid_parse.mlir.

// Valid: binder tvar use + lambda region protocols.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %f = "tlam.vlambda"() ({
    ^bb1(%x: !value<%T>):
      "tlam.vreturn"(%x) : (!value<%T>) -> ()
    }) : () -> !tlam.fun<!value<%T>, !value<%T>>
    "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.tlambda"() ({
// CHECK:   ^bb0(%1: !tlam.type):
// CHECK:     %2 = "tlam.vlambda"() ({
// CHECK:     ^bb1(%3: !value<%1>):
// CHECK:       "tlam.vreturn"(%3) : (!value<%1>) -> ()
// CHECK:     }) : () -> !tlam.fun<!value<%1>, !value<%1>>
// CHECK:     "tlam.treturn"(%2) : (!tlam.fun<!value<%1>, !value<%1>>) -> ()
// CHECK:   }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
// CHECK: }

// -----

// Invalid: tapply operand is not forall.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %h = "tlam.tapply"(%x) <{tyArg = i64}> : (i32) -> i64
}

// CHECK: tapply: operand must have !tlam.forall type, got i32

// -----

// Invalid: tapply result type mismatch.
builtin.module {
  %G = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "builtin.unrealized_conversion_cast"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %h = "tlam.tapply"(%G) <{tyArg = i32}>
       : (!tlam.forall<i64>) -> i32
}

// CHECK: tapply: result i32 != instantiated i64

// -----

// Invalid: vapply callee is not fun.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %r = "tlam.vapply"(%x, %x) : (i32, i32) -> i32
}

// CHECK: vapply: first operand must have !tlam.fun type, got i32

// -----

// Invalid: vapply arg type mismatch.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 0 : i64}> : () -> i64
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i64) -> i32
}

// CHECK: vapply: expected arg i32 and result i32, got i64 and i32

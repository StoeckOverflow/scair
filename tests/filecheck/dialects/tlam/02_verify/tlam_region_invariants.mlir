// Purpose: Verifier checks for TLam region shape and terminator placement.
// Invariants covered: TLambda/Vlambda single-block protocol and terminator-last requirements.

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Targets: explicit region/terminator protocol checks for TLam ops.

// Invalid: treturn not last in tlambda body.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "test.make_i64"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
    %late = "test.make_i64"() : () -> i64
  }) : () -> !tlam.forall<i64>
}

// CHECK: Operation 'tlam.treturn' marked as a terminator, but is not the last operation within its container block

// -----

// Invalid: vreturn not last in vlambda body.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
    %late = "arith.constant"() <{value = 0 : i32}> : () -> i32
  }) : () -> !tlam.fun<i32, i32>
}

// CHECK: Operation 'tlam.vreturn' marked as a terminator, but is not the last operation within its container block

// -----

// Invalid: tlambda block arg wrong type.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: i32):
    %v = "test.make_i64"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>
}

// CHECK: tlambda: binder block argument must have type !tlam.type, got i32

// -----

// Invalid: vlambda block arg type must match function input.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i64, i64>
}

// CHECK: vreturn: expected value type i64 from enclosing vlambda, got i32

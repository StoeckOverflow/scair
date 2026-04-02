// Purpose: Parser diagnostics for malformed TLam SSA-in-types syntax/typing at parse time.
// Invariants covered: Reject wrong-type SSA references in tvar payloads and malformed tapply type args.

// RUN: scair-opt %s --allow-unregistered-dialect --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Invalid: tvar references SSA value that is not !tlam.type.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %y = "test.make"() : () -> !value<%x>
}

// CHECK: builtin.module {
// CHECK:   %0 = "arith.constant"() <{value = 0 : i32}> : () -> i32
// CHECK:   %1 = "test.make"() : () -> !value<%0>
// CHECK: }

// -----

// Invalid: tapply tyArg uses tvar whose SSA reference is not !tlam.type.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %G = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %v = "test.make_i64"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %h = "tlam.tapply"(%G) <{tyArg = !value<%x>}>
       : (!tlam.forall<i64>) -> i64
}

// CHECK: builtin.module {
// CHECK:   %0 = "arith.constant"() <{value = 0 : i32}> : () -> i32
// CHECK:   %1 = "tlam.tlambda"() ({
// CHECK:   ^bb0(%2: !tlam.type):
// CHECK:     %3 = "test.make_i64"() : () -> i64
// CHECK:     "tlam.treturn"(%3) : (i64) -> ()
// CHECK:   }) : () -> !tlam.forall<i64>
// CHECK:   %2 = "tlam.tapply"(%1) <{tyArg = !value<%0>}> : (!tlam.forall<i64>) -> i64
// CHECK: }

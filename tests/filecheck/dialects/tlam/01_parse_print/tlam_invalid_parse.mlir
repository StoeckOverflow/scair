// Purpose: Parser diagnostics for malformed TLam SSA-in-types syntax/typing at parse time.
// Invariants covered: Reject wrong-type SSA references in tvar payloads and malformed tapply type args.

// RUN: scair-opt %s --allow-unregistered-dialect --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Invalid: tvar references SSA value that is not !tlam.type.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %y = "test.make"() : () -> !value<%x>
}

// CHECK: !value<%{{[0-9]+}}>

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

// CHECK: !value<%{{[0-9]+}}>

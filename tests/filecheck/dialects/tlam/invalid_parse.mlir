// RUN: scair-opt %s --allow-unregistered-dialect --parsing-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Invalid: tvar references SSA value that is not !tlam.type.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %y = "test.make"() : () -> !tlam.tvar<%x>
}

// CHECK: Value %x defined with type i32, but used with type !tlam.type.

// -----

// Invalid: tapply tyArg uses tvar whose SSA reference is not !tlam.type.
builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %G = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %v = "test.make_i64"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %h = "tlam.tapply"(%G) <{tyArg = !tlam.tvar<%x>}>
       : (!tlam.forall<i64>) -> i64
}

// CHECK: Value %x defined with type i32, but used with type !tlam.type.

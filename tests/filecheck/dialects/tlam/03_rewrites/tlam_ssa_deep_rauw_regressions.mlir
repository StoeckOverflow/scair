// Purpose: Deep RAUW stress tests for embedded tvar references across IR locations.
// Invariants covered: Rewrites update tvar in result/operand types, attrs, and nested payloads.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p cse --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=CSE

// Targets: deep RAUW for embedded Tvar references in multiple locations.

builtin.module {
  %x = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %T0 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  %T1 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type

  // Result type carries tvar<%T1>.
  %r = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !value<%T1>

  // Operand type carries tvar<%T1>.
  "test.consume"(%r) : (!value<%T1>) -> ()

  // Attribute payload carries nested tvar<%T1> in forall/fun.
  "test.use"(%r) {dep = !tlam.forall<!tlam.fun<!value<%T1>, !tlam.forall<!value<%T1>>>>}
    : (!value<%T1>) -> ()

  // Property payload carries tvar<%T1>.
  %G = "tlam.tlambda"() ({
  ^bb0(%U: !tlam.type):
    %v = "test.make_i64"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>

  %h = "tlam.tapply"(%G) <{tyArg = !value<%T1>}>
       : (!tlam.forall<i64>) -> i64
}

// CSE-LABEL: builtin.module {
// CSE: "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !tlam.type
// CSE: "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (i32) -> !value<%{{[0-9]+}}>
// CSE: "test.consume"(%{{[0-9]+}}) : (!value<%{{[0-9]+}}>) -> ()
// CSE: "test.use"(%{{[0-9]+}}) {dep = !tlam.forall<!tlam.fun<!value<%{{[0-9]+}}>, !tlam.forall<!value<%{{[0-9]+}}>>>>}
// CSE: "tlam.tapply"(%{{[0-9]+}}) <{tyArg = !value<%{{[0-9]+}}>}> : (!tlam.forall<i64>) -> i64
// CSE: }

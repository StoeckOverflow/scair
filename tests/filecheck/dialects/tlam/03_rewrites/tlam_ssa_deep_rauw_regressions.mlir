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

// CSE: builtin.module {
// CSE:   %0 = "arith.constant"() <{value = 0 : i32}> : () -> i32
// CSE:   %1 = "builtin.unrealized_conversion_cast"(%0) : (i32) -> !tlam.type
// CSE:   %2 = "builtin.unrealized_conversion_cast"(%0) : (i32) -> !value<%1>
// CSE:   "test.consume"(%2) : (!value<%1>) -> ()
// CSE:   "test.use"(%2) {dep = !tlam.forall<!tlam.fun<!value<%1>, !tlam.forall<!value<%1>>>>} : (!value<%1>) -> ()
// CSE:   %3 = "tlam.tlambda"() ({
// CSE:   ^bb0(%4: !tlam.type):
// CSE:     %5 = "test.make_i64"() : () -> i64
// CSE:     "tlam.treturn"(%5) : (i64) -> ()
// CSE:   }) : () -> !tlam.forall<i64>
// CSE:   %4 = "tlam.tapply"(%3) <{tyArg = !value<%1>}> : (!tlam.forall<i64>) -> i64
// CSE: }

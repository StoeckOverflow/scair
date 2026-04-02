// Purpose: CSE isolation-boundary regression coverage.
// Invariants covered: CSE does not merge values across isolated region boundaries; local same-block CSE still works.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p cse --verify-diagnostics | filecheck %s -DFILE=%s --check-prefix=ISO

// Cross-function (isolated) values must not be CSE'd together.
builtin.module {
  func.func @left() -> !tlam.type {
    %x = "arith.constant"() <{value = 1 : i32}> : () -> i32
    %t = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
    func.return %t : !tlam.type
  }

  func.func @right() -> !tlam.type {
    %x = "arith.constant"() <{value = 1 : i32}> : () -> i32
    %t = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
    func.return %t : !tlam.type
  }
}

// ISO: builtin.module {
// ISO:   func.func @left() -> !tlam.type {
// ISO:     %0 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// ISO:     %1 = "builtin.unrealized_conversion_cast"(%0) : (i32) -> !tlam.type
// ISO:     func.return %1 : !tlam.type
// ISO:   }
// ISO:   func.func @right() -> !tlam.type {
// ISO:     %0 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// ISO:     %1 = "builtin.unrealized_conversion_cast"(%0) : (i32) -> !tlam.type
// ISO:     func.return %1 : !tlam.type
// ISO:   }
// ISO: }

// -----

// Control: within one block, equivalent casts should CSE.
builtin.module {
  %x = "arith.constant"() <{value = 3 : i32}> : () -> i32
  %t0 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  %t1 = "builtin.unrealized_conversion_cast"(%x) : (i32) -> !tlam.type
  "test.use"() {a = !value<%t0>, b = !value<%t1>} : () -> ()
}

// ISO: builtin.module {
// ISO:   %0 = "arith.constant"() <{value = 3 : i32}> : () -> i32
// ISO:   %1 = "builtin.unrealized_conversion_cast"(%0) : (i32) -> !tlam.type
// ISO:   "test.use"() {a = !value<%1>, b = !value<%1>} : () -> ()
// ISO: }

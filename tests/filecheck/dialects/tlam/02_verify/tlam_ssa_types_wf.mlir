// Purpose: Comprehensive WF verifier coverage for TLam SSA-in-types core invariants.
// Invariants covered: Dominance-in-types, DBI bounds, region protocols, and apply typing rules.

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s -DFILE=%s

// Targets: verifier WF for SSA-in-types TLam (dominance-in-types, DBI bounds,
// region/terminator protocols, and apply typing checks).

// Note: wrong-type tvar parse-time failures are covered in
// tests/filecheck/dialects/tlam/invalid_parse.mlir.

// Valid: binder tvar use + lambda region protocols.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %f = "tlam.vlambda"() ({
    ^bb1(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%T>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>
    "tlam.treturn"(%f) : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
}

// CHECK-LABEL: builtin.module {
// CHECK: "tlam.tlambda"()
// CHECK: "tlam.vlambda"()
// CHECK: "tlam.vreturn"
// CHECK: "tlam.treturn"
// CHECK: }

// -----

// Valid (policy): any dominating !tlam.type SSA value is allowed in tvar.
builtin.module {
  %T = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
  %x = "test.make"() : () -> i64
  "test.use"(%x) {dep = !tlam.forall<!tlam.tvar<%T>>} : (i64) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: "builtin.unrealized_conversion_cast"()
// CHECK: "test.use"(%{{[0-9]+}}) {dep = !tlam.forall<!tlam.tvar
// CHECK: }

// -----

// Invalid: non-dominating tvar use nested under forall.
builtin.module {
  "test.use"() {dep = !tlam.forall<!tlam.tvar<%T>>} : () -> ()
  %T = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
}

// CHECK: ssa-dominance: value Value{{.*}} does not dominate its use in op `test.use`

// -----

// Invalid: bvar out of scope at depth 0.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.bvar<0>):
    "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
  }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>
}

// CHECK: debruijn: bvar<0> out of scope at depth=0

// -----

// Invalid: tlambda with wrong block argument count.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0():
    %v = "builtin.unrealized_conversion_cast"() : () -> i64
    "tlam.treturn"(%v) : (i64) -> ()
  }) : () -> !tlam.forall<i64>
}

// CHECK: tlambda: must have exactly one block with one arg

// -----

// Invalid: tlambda missing treturn terminator.
builtin.module {
  %F = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %v = "builtin.unrealized_conversion_cast"() : () -> i64
  }) : () -> !tlam.forall<i64>
}

// CHECK: tlambda: last op must be tlam.treturn

// -----

// Invalid: vlambda missing vreturn terminator.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %z = "arith.constant"() <{value = 0 : i32}> : () -> i32
  }) : () -> !tlam.fun<i32, i32>
}

// CHECK: vlambda: last op must be tlam.vreturn

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

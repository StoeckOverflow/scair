// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam --verify-diagnostics | filecheck %s -DFILE=%s

// Targets: value beta-reduction correctness for DB-style TLam, including
// capture avoidance, conservative non-reduction, and verifier-safe rewriting.

// Valid 1: identity beta-reduces.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 7 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
  "test.use"(%r) : (i32) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK-NOT: "tlam.vapply"
// CHECK: "test.use"(%{{[0-9]+}}) : (i32) -> ()
// CHECK: }

// -----

// Valid 2: nested vlambda shadowing is capture-avoiding.
builtin.module {
  %make = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %inner = "tlam.vlambda"() ({
    ^bb1(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> !tlam.fun<i32, i32>
    "tlam.vreturn"(%inner) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> !tlam.fun<i32, !tlam.fun<i32, i32>>

  %a = "arith.constant"() <{value = 11 : i32}> : () -> i32
  %g = "tlam.vapply"(%make, %a) : (!tlam.fun<i32, !tlam.fun<i32, i32>>, i32) -> !tlam.fun<i32, i32>
  "test.use_fun"(%g) : (!tlam.fun<i32, i32>) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK-NOT: "tlam.vapply"
// CHECK: "tlam.vlambda"() ({
// CHECK: ^bb{{[0-9]+}}(%{{[0-9]+}}: i32):
// CHECK: "tlam.vreturn"(%{{[0-9]+}}) : (i32) -> ()
// CHECK: }) : () -> !tlam.fun<i32, i32>
// CHECK: "test.use_fun"(%{{[0-9]+}}) : (!tlam.fun<i32, i32>) -> ()
// CHECK: }

// -----

// Valid 3: body with pure intermediates is cloned and rewired.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %c1 = "arith.constant"() <{value = 1 : i32}> : () -> i32
    %sum = "arith.addi"(%x, %c1) : (i32, i32) -> i32
    "tlam.vreturn"(%sum) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 9 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
  "test.use"(%r) : (i32) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK: "arith.addi"(%{{[0-9]+}}, %{{[0-9]+}}) : (i32, i32) -> i32
// CHECK-NOT: "tlam.vapply"
// CHECK: "test.use"(%{{[0-9]+}}) : (i32) -> ()
// CHECK: }

// -----

// Must NOT reduce 4: callee is not directly produced by vlambda.
builtin.module {
  %f = "test.fun_source"() : () -> !tlam.fun<i32, i32>
  %a = "arith.constant"() <{value = 3 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// CHECK-LABEL: builtin.module {
// CHECK: "tlam.vapply"
// CHECK: }

// -----

// Must NOT reduce 5: lambda body contains effectful/unknown op.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %e = "test.effect"(%x) : (i32) -> i32
    "tlam.vreturn"(%e) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 5 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// CHECK-LABEL: builtin.module {
// CHECK: "test.effect"
// CHECK: "tlam.vapply"
// CHECK: }

// -----

// Must NOT reduce 6: effectful arg producer used more than once in body.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %sum = "arith.addi"(%x, %x) : (i32, i32) -> i32
    "tlam.vreturn"(%sum) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "test.effect_i32"() : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// CHECK-LABEL: builtin.module {
// CHECK: "test.effect_i32"() : () -> i32
// CHECK: "tlam.vapply"
// CHECK: }

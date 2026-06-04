// Purpose: Value beta-reduction rewrite behavior for TLam with SSA-in-types.
// Invariants covered: Deep type rewrites for embedded tvar SSA refs and dominance-preserving inlining.

// RUN: scair-opt %s --allow-unregistered-dialect --split-input-file -p beta-reduce-tlam --verify-diagnostics | filecheck %s -DFILE=%s

// Targets: beta-reduction with SSA-in-types, including deep rewrites of
// embedded tvar SSA references and dominance preservation.

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

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     "tlam.vreturn"(%1) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 7 : i32}> : () -> i32
// CHECK:   "test.use"(%1) : (i32) -> ()
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

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "tlam.vlambda"() ({
// CHECK:     ^bb1(%3: i32):
// CHECK:       "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:     }) : () -> !tlam.fun<i32, i32>
// CHECK:     "tlam.vreturn"(%2) : (!tlam.fun<i32, i32>) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, !tlam.fun<i32, i32>>
// CHECK:   %1 = "arith.constant"() <{value = 11 : i32}> : () -> i32
// CHECK:   %2 = "tlam.vlambda"() ({
// CHECK:   ^bb1(%3: i32):
// CHECK:     "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   "test.use_fun"(%2) : (!tlam.fun<i32, i32>) -> ()
// CHECK: }

// -----

// Valid 3: body with memory-effect-free intermediates is cloned at call site.
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

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK:     %3 = "arith.addi"(%1, %2) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
// CHECK:     "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 9 : i32}> : () -> i32
// CHECK:   %2 = "arith.constant"() <{value = 1 : i32}> : () -> i32
// CHECK:   %3 = "arith.addi"(%1, %2) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
// CHECK:   "test.use"(%3) : (i32) -> ()
// CHECK: }

// -----

// Valid 4: SSA-in-types remap in cloned ops: !value<%x> becomes !value<%A>.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.type):
    %tv = "builtin.unrealized_conversion_cast"(%x) : (!tlam.type) -> !value<%x>
    %back = "builtin.unrealized_conversion_cast"(%tv) : (!value<%x>) -> !tlam.type
    "tlam.vreturn"(%back) : (!tlam.type) -> ()
  }) : () -> !tlam.fun<!tlam.type, !tlam.type>

  %A = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
  %r = "tlam.vapply"(%f, %A) : (!tlam.fun<!tlam.type, !tlam.type>, !tlam.type) -> !tlam.type
  "test.use"(%r) : (!tlam.type) -> ()
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: !tlam.type):
// CHECK:     %2 = "builtin.unrealized_conversion_cast"(%1) : (!tlam.type) -> !value<%1>
// CHECK:     %3 = "builtin.unrealized_conversion_cast"(%2) : (!value<%1>) -> !tlam.type
// CHECK:     "tlam.vreturn"(%3) : (!tlam.type) -> ()
// CHECK:   }) : () -> !tlam.fun<!tlam.type, !tlam.type>
// CHECK:   %1 = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
// CHECK:   %2 = "builtin.unrealized_conversion_cast"(%1) : (!tlam.type) -> !value<%1>
// CHECK:   %3 = "builtin.unrealized_conversion_cast"(%2) : (!value<%1>) -> !tlam.type
// CHECK:   "test.use"(%3) : (!tlam.type) -> ()
// CHECK: }

// -----

// Must NOT reduce 5: callee is not directly a vlambda producer.
builtin.module {
  %f = "test.fun_source"() : () -> !tlam.fun<i32, i32>
  %a = "arith.constant"() <{value = 3 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// CHECK: builtin.module {
// CHECK:   %0 = "test.fun_source"() : () -> !tlam.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 3 : i32}> : () -> i32
// CHECK:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// CHECK: }

// -----

// Must NOT reduce 6: lambda body contains effectful/unknown op.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %e = "test.effect"(%x) : (i32) -> i32
    "tlam.vreturn"(%e) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 5 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "test.effect"(%1) : (i32) -> i32
// CHECK:     "tlam.vreturn"(%2) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   %1 = "arith.constant"() <{value = 5 : i32}> : () -> i32
// CHECK:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// CHECK: }

// -----

// Must NOT reduce 6b: lambda body contains a call (vapply), conservatively effectful.
builtin.module {
  %callee = "test.fun_source"() : () -> !tlam.fun<i32, i32>
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %y = "tlam.vapply"(%callee, %x) : (!tlam.fun<i32, i32>, i32) -> i32
    "tlam.vreturn"(%y) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 5 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// CHECK: builtin.module {
// CHECK:   %0 = "test.fun_source"() : () -> !tlam.fun<i32, i32>
// CHECK:   %1 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%2: i32):
// CHECK:     %3 = "tlam.vapply"(%0, %2) : (!tlam.fun<i32, i32>, i32) -> i32
// CHECK:     "tlam.vreturn"(%3) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   %2 = "arith.constant"() <{value = 5 : i32}> : () -> i32
// CHECK:   %3 = "tlam.vapply"(%1, %2) : (!tlam.fun<i32, i32>, i32) -> i32
// CHECK: }

// -----

// Must NOT reduce 7: effectful arg producer used more than once in body.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    %sum = "arith.addi"(%x, %x) : (i32, i32) -> i32
    "tlam.vreturn"(%sum) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "test.effect_i32"() : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: i32):
// CHECK:     %2 = "arith.addi"(%1, %1) <{overflowFlags = #arith.overflow<none>}> : (i32, i32) -> i32
// CHECK:     "tlam.vreturn"(%2) : (i32) -> ()
// CHECK:   }) : () -> !tlam.fun<i32, i32>
// CHECK:   %1 = "test.effect_i32"() : () -> i32
// CHECK:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i32, i32>, i32) -> i32
// CHECK: }

// -----

// Must NOT reduce 8: effectful body with uses-in-types.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.type):
    %e = "test.effect_type"(%x) : (!tlam.type) -> !value<%x>
    %back = "builtin.unrealized_conversion_cast"(%e) : (!value<%x>) -> !tlam.type
    "tlam.vreturn"(%back) : (!tlam.type) -> ()
  }) : () -> !tlam.fun<!tlam.type, !tlam.type>

  %A = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
  %r = "tlam.vapply"(%f, %A) : (!tlam.fun<!tlam.type, !tlam.type>, !tlam.type) -> !tlam.type
}

// CHECK: builtin.module {
// CHECK:   %0 = "tlam.vlambda"() ({
// CHECK:   ^bb0(%1: !tlam.type):
// CHECK:     %2 = "test.effect_type"(%1) : (!tlam.type) -> !value<%1>
// CHECK:     %3 = "builtin.unrealized_conversion_cast"(%2) : (!value<%1>) -> !tlam.type
// CHECK:     "tlam.vreturn"(%3) : (!tlam.type) -> ()
// CHECK:   }) : () -> !tlam.fun<!tlam.type, !tlam.type>
// CHECK:   %1 = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
// CHECK:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<!tlam.type, !tlam.type>, !tlam.type) -> !tlam.type
// CHECK: }

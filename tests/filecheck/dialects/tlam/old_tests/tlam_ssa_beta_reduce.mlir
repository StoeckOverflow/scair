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

// CHECK-LABEL: builtin.module {
// CHECK-NOT: "tlam.vapply"
// CHECK: "test.use"(%{{[0-9]+}}) : (i32) -> ()
// CHECK: }

// -----

// Valid 2: body with memory-effect-free intermediates is cloned at call site.
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

// Valid 3: SSA-in-types remap in cloned ops: !tlam.tvar<%x> becomes !tlam.tvar<%A>.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.type):
    %tv = "builtin.unrealized_conversion_cast"(%x) : (!tlam.type) -> !tlam.tvar<%x>
    %back = "builtin.unrealized_conversion_cast"(%tv) : (!tlam.tvar<%x>) -> !tlam.type
    "tlam.vreturn"(%back) : (!tlam.type) -> ()
  }) : () -> !tlam.fun<!tlam.type, !tlam.type>

  %A = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
  %r = "tlam.vapply"(%f, %A) : (!tlam.fun<!tlam.type, !tlam.type>, !tlam.type) -> !tlam.type
  "test.use"(%r) : (!tlam.type) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: [[A:%[0-9]+]] = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
// CHECK: "builtin.unrealized_conversion_cast"([[A]]) : (!tlam.type) -> !tlam.tvar<[[A]]>
// CHECK-NOT: "tlam.vapply"
// CHECK: "test.use"(%{{[0-9]+}}) : (!tlam.type) -> ()
// CHECK: }

// -----

// Must NOT reduce 4: callee is not directly a vlambda producer.
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

// -----

// Valid 7: dominance-in-types remains valid after replacing vapply result.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.type):
    %tv = "builtin.unrealized_conversion_cast"(%x) : (!tlam.type) -> !tlam.tvar<%x>
    %back = "builtin.unrealized_conversion_cast"(%tv) : (!tlam.tvar<%x>) -> !tlam.type
    "tlam.vreturn"(%back) : (!tlam.type) -> ()
  }) : () -> !tlam.fun<!tlam.type, !tlam.type>

  %A = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
  %r = "tlam.vapply"(%f, %A) : (!tlam.fun<!tlam.type, !tlam.type>, !tlam.type) -> !tlam.type
  "test.use"(%r) {dep = !tlam.tvar<%r>} : (!tlam.type) -> ()
}

// CHECK-LABEL: builtin.module {
// CHECK: [[A:%[0-9]+]] = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
// CHECK: "builtin.unrealized_conversion_cast"([[A]]) : (!tlam.type) -> !tlam.tvar<[[A]]>
// CHECK-NOT: "tlam.vapply"
// CHECK: "test.use"(%{{[0-9]+}}) {dep = !tlam.tvar<%{{[0-9]+}}>}
// CHECK: }

// -----

// Must NOT reduce 8: effectful body with uses-in-types.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: !tlam.type):
    %e = "test.effect_type"(%x) : (!tlam.type) -> !tlam.tvar<%x>
    %back = "builtin.unrealized_conversion_cast"(%e) : (!tlam.tvar<%x>) -> !tlam.type
    "tlam.vreturn"(%back) : (!tlam.type) -> ()
  }) : () -> !tlam.fun<!tlam.type, !tlam.type>

  %A = "builtin.unrealized_conversion_cast"() : () -> !tlam.type
  %r = "tlam.vapply"(%f, %A) : (!tlam.fun<!tlam.type, !tlam.type>, !tlam.type) -> !tlam.type
}

// CHECK-LABEL: builtin.module {
// CHECK: "test.effect_type"
// CHECK: "tlam.vapply"
// CHECK: }

// -----

// Valid 9: nested tlambda/vlambda remains well-formed with binder tvar.
builtin.module {
  %poly = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x) : (!tlam.tvar<%T>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>

    %arg = "builtin.unrealized_conversion_cast"(%T) : (!tlam.type) -> !tlam.tvar<%T>
    %r = "tlam.vapply"(%id, %arg) : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>, !tlam.tvar<%T>) -> !tlam.tvar<%T>
    "tlam.treturn"(%r) : (!tlam.tvar<%T>) -> ()
  }) : () -> !tlam.forall<!tlam.bvar<0>>
}

// CHECK-LABEL: builtin.module {
// CHECK: "tlam.tlambda"()
// CHECK: "builtin.unrealized_conversion_cast"(%{{[0-9]+}}) : (!tlam.type) -> !tlam.tvar<%{{[0-9]+}}>
// CHECK-NOT: "tlam.vapply"
// CHECK: "tlam.treturn"(%{{[0-9]+}}) : (!tlam.tvar<%{{[0-9]+}}>) -> ()
// CHECK: }

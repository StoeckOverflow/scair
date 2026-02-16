// RUN: scair-opt %s --allow-unregistered-dialect -p lower-tlam-to-func --split-input-file | filecheck %s -DFILE=%s

// Valid: lower vapply by first lifting vlambda, then rewriting to func.call_indirect.
builtin.module {
  %f = "tlam.vlambda"() ({
  ^bb0(%x: i32):
    "tlam.vreturn"(%x) : (i32) -> ()
  }) : () -> !tlam.fun<i32, i32>

  %a = "arith.constant"() <{value = 0 : i32}> : () -> i32
  %r = "tlam.vapply"(%f, %a) : (!tlam.fun<i32, i32>, i32) -> i32
}

// CHECK-LABEL: builtin.module {
// CHECK-DAG: func.func @lifted_
// CHECK-DAG: %{{[0-9]+}} = func.constant @lifted_
// CHECK: %{{[0-9]+}} = "func.call_indirect"
// CHECK-NOT: "tlam.vlambda"
// CHECK-NOT: "tlam.vapply"
// CHECK-NOT: "tlam.vreturn"
// CHECK: }

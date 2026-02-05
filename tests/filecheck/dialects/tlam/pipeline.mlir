// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize,erase-tlam,lower-tlam-to-func --split-input-file | filecheck %s -DFILE=%s -dump-input=always

// Simple polymorphic identity through full pipeline.
builtin.module {
  %0 = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %poly_id = "tlam.tlambda"() ({
    ^bb1(%U: !tlam.type):
      %id = "tlam.vlambda"() ({
      ^bb2(%x: !tlam.tvar<%U>):
        "tlam.vreturn"(%x): (!tlam.tvar<%U>) -> ()
      }) : () -> !tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.tvar<%U>, !tlam.tvar<%U>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

    %spec = "tlam.tapply"(%poly_id) <{tyArg = i64}>
           : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%spec) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<i64, i64>>
}

// CHECK-LABEL: builtin.module {
// CHECK-NOT: "tlam."
// CHECK: func.func @lifted_{{[0-9]+}}(%0: i64) -> i64 {
// CHECK-NEXT: func.return %0 : i64
// CHECK-NEXT: }
// CHECK: %{{[0-9]+}} = func.constant @lifted_{{[0-9]+}} : (i64) -> i64
// CHECK: }

// -----

// Nested tapply chain through the full pipeline.
builtin.module {
  %make_id = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.tvar<%T>):
      "tlam.vreturn"(%x): (!tlam.tvar<%T>) -> ()
    }) : () -> !tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.tvar<%T>, !tlam.tvar<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %id_i64 = "tlam.tapply"(%make_id) <{tyArg = i64}>
           : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
  %id_i32 = "tlam.tapply"(%make_id) <{tyArg = i32}>
           : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i32, i32>)
}

// CHECK-LABEL: builtin.module {
// CHECK-NOT: "tlam."
// CHECK-DAG: func.func @lifted_{{[0-9]+}}(%0: i64) -> i64 {
// CHECK-DAG: func.func @lifted_{{[0-9]+}}(%0: i32) -> i32 {
// CHECK-DAG: func.constant @lifted_{{[0-9]+}} : (i64) -> i64
// CHECK-DAG: func.constant @lifted_{{[0-9]+}} : (i32) -> i32
// CHECK: }

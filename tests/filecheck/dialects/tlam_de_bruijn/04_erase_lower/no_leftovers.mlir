// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam | filecheck %s --check-prefix=ERASE
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam,lower-tlam-to-func | filecheck %s --check-prefix=LOWER

builtin.module {
  %outer = "tlam.tlambda"() ({
    %poly_id = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x): (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    %spec = "tlam.tapply"(%poly_id) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    %x = "arith.constant"() <{value = 1 : i64}> : () -> (i64)
    %r = "tlam.vapply"(%spec, %x) : (!tlam.fun<i64, i64>, i64) -> (i64)
    %f = "tlam.vlambda"() ({
    ^bb0(%y: i64):
      "tlam.vreturn"(%y) : (i64) -> ()
    }) : () -> (!tlam.fun<i64, i64>)
    %r2 = "tlam.vapply"(%f, %r) : (!tlam.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r2) : (i64) -> ()
    "tlam.treturn"(%spec) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}

// ERASE-NOT: "tlam.tlambda"
// ERASE-NOT: "tlam.tapply"
// ERASE-NOT: "tlam.treturn"
// ERASE: "tlam.vlambda"
// ERASE: "tlam.vapply"

// LOWER-NOT: "tlam.tlambda"
// LOWER-NOT: "tlam.tapply"
// LOWER-NOT: "tlam.vlambda"
// LOWER-NOT: "tlam.vapply"
// LOWER-DAG: func.func
// LOWER-DAG: "func.call_indirect"
// LOWER-DAG: func.return

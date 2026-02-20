// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam | filecheck %s --check-prefix=ERASE -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam,lower-tlam-to-func | filecheck %s --check-prefix=LOWER -DFILE=%s

// EXTRA STRICTNESS: erase/lower should remove all TLam constructs in this shape too.
builtin.module {
  %outer = "tlam.tlambda"() ({
    %poly = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    %spec = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    %c = "arith.constant"() <{value = 9 : i64}> : () -> (i64)
    %r = "tlam.vapply"(%spec, %c) : (!tlam.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()

    %f2 = "tlam.vlambda"() ({
    ^bb0(%z: i64):
      "tlam.vreturn"(%z) : (i64) -> ()
    }) : () -> (!tlam.fun<i64, i64>)
    %r2 = "tlam.vapply"(%f2, %r) : (!tlam.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r2) : (i64) -> ()

    "tlam.treturn"(%spec) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}

// ERASE-NOT: "tlam.tlambda"
// ERASE-NOT: "tlam.tapply"
// ERASE-NOT: "tlam.treturn"
// ERASE: "tlam.vlambda"
// ERASE: "tlam.vapply"

// LOWER-NOT: "tlam."
// LOWER: func.func
// LOWER: "func.call_indirect"

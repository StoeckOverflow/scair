// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam,canonicalize,cse,monomorphize,erase-tlam,lower-tlam-to-func,canonicalize,cse | filecheck %s --check-prefix=FULL -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam,canonicalize,cse,monomorphize,erase-tlam,lower-tlam-to-func,canonicalize,cse,beta-reduce-tlam,canonicalize,cse,monomorphize,erase-tlam,lower-tlam-to-func,canonicalize,cse | filecheck %s --check-prefix=FULL2 -DFILE=%s

// Build two specializations, use one; pipeline should remain stable and TLam-free.
builtin.module {
  %outer = "tlam.tlambda"() ({
    %poly = "tlam.tlambda"() ({
      %id = "tlam.vlambda"() ({
      ^bb0(%x: !tlam.bvar<0>):
        "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
      }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
      "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
    }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

    %a = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
    %b = "tlam.tapply"(%poly) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i32, i32>)
    %c = "arith.constant"() <{value = 6 : i64}> : () -> (i64)
    %r = "tlam.vapply"(%a, %c) : (!tlam.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()

    %id2 = "tlam.vlambda"() ({
    ^bb0(%z: i64):
      "tlam.vreturn"(%z) : (i64) -> ()
    }) : () -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%id2) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
}

// FULL-NOT: "tlam."
// FULL: func.func
// FULL2-NOT: "tlam."
// FULL2: func.func

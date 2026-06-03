// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p canonicalize,cse,monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,canonicalize,cse | filecheck %s -DFILE=%s

// Pipeline smoke with cse/canonicalize around TLam lowering.
builtin.module {
  %outer = "tlam_dbi.tlambda"() ({
    %poly = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

    %a = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    %b = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)

    %c = "arith.constant"() <{value = 21 : i64}> : () -> (i64)
    %r = "tlam_dbi.vapply"(%a, %c) : (!tlam_dbi.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()

    %id2 = "tlam_dbi.vlambda"() ({
    ^bb0(%z: i64):
      "tlam_dbi.vreturn"(%z) : (i64) -> ()
    }) : () -> (!tlam_dbi.fun<i64, i64>)
    "tlam_dbi.treturn"(%id2) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
  %top = "tlam_dbi.tapply"(%outer) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>) -> (!tlam_dbi.fun<i64, i64>)
  "test.use"(%top) : (!tlam_dbi.fun<i64, i64>) -> ()
}

// CHECK: builtin.module {
// CHECK:   func.func @lifted_2(%0: i64) -> i64 {
// CHECK:     func.return %0 : i64
// CHECK:   }
// CHECK:   %0 = func.constant @lifted_2 : (i64) -> i64
// CHECK:   func.func @lifted_1(%1: i64) -> i64 {
// CHECK:     func.return %1 : i64
// CHECK:   }
// CHECK:   %1 = func.constant @lifted_1 : (i64) -> i64
// CHECK:   %2 = "arith.constant"() <{value = 21}> : () -> i64
// CHECK:   %3 = "func.call_indirect"(%1, %2) : ((i64) -> i64, i64) -> i64
// CHECK:   "test.use"(%3) : (i64) -> ()
// CHECK: }

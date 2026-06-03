// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam-de-bruijn,canonicalize,cse,monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,canonicalize,cse | filecheck %s --check-prefix=FULL -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam-de-bruijn,canonicalize,cse,monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,canonicalize,cse,beta-reduce-tlam-de-bruijn,canonicalize,cse,monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,canonicalize,cse | filecheck %s --check-prefix=FULL2 -DFILE=%s

// Build two specializations, use one; pipeline should remain stable and TLam-free.
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
    %b = "tlam_dbi.tapply"(%poly) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i32, i32>)
    %c = "arith.constant"() <{value = 6 : i64}> : () -> (i64)
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

// FULL: builtin.module {
// FULL:   func.func @lifted_2(%0: i64) -> i64 {
// FULL:     func.return %0 : i64
// FULL:   }
// FULL:   %0 = func.constant @lifted_2 : (i64) -> i64
// FULL:   func.func @lifted_1(%1: i64) -> i64 {
// FULL:     func.return %1 : i64
// FULL:   }
// FULL:   %1 = func.constant @lifted_1 : (i64) -> i64
// FULL:   %2 = "arith.constant"() <{value = 6}> : () -> i64
// FULL:   %3 = "func.call_indirect"(%1, %2) : ((i64) -> i64, i64) -> i64
// FULL:   "test.use"(%3) : (i64) -> ()
// FULL: }
// FULL2: builtin.module {
// FULL2:   func.func @lifted_2(%0: i64) -> i64 {
// FULL2:     func.return %0 : i64
// FULL2:   }
// FULL2:   %0 = func.constant @lifted_2 : (i64) -> i64
// FULL2:   func.func @lifted_1(%1: i64) -> i64 {
// FULL2:     func.return %1 : i64
// FULL2:   }
// FULL2:   %1 = func.constant @lifted_1 : (i64) -> i64
// FULL2:   %2 = "arith.constant"() <{value = 6}> : () -> i64
// FULL2:   %3 = "func.call_indirect"(%1, %2) : ((i64) -> i64, i64) -> i64
// FULL2:   "test.use"(%3) : (i64) -> ()
// FULL2: }

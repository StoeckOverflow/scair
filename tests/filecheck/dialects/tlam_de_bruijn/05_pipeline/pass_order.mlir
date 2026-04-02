// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam-de-bruijn,monomorphize-tlam-de-bruijn,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,canonicalize,cse | filecheck %s --check-prefix=ORDER1 -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,beta-reduce-tlam-de-bruijn,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func,canonicalize,cse | filecheck %s --check-prefix=ORDER2 -DFILE=%s

builtin.module {
  %outer = "tlam_dbi.tlambda"() ({
    %poly = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
    %spec = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    %c = "arith.constant"() <{value = 5 : i64}> : () -> (i64)
    %r = "tlam_dbi.vapply"(%spec, %c) : (!tlam_dbi.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()
    %f2 = "tlam_dbi.vlambda"() ({
    ^bb0(%z: i64):
      "tlam_dbi.vreturn"(%z) : (i64) -> ()
    }) : () -> (!tlam_dbi.fun<i64, i64>)
    "tlam_dbi.treturn"(%f2) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
}

// ORDER1: builtin.module {
// ORDER1:   func.func @lifted_2(%0: i64) -> i64 {
// ORDER1:     func.return %0 : i64
// ORDER1:   }
// ORDER1:   %0 = func.constant @lifted_2 : (i64) -> i64
// ORDER1:   func.func @lifted_1(%1: i64) -> i64 {
// ORDER1:     func.return %1 : i64
// ORDER1:   }
// ORDER1:   %1 = func.constant @lifted_1 : (i64) -> i64
// ORDER1:   %2 = "arith.constant"() <{value = 5}> : () -> i64
// ORDER1:   %3 = "func.call_indirect"(%1, %2) : ((i64) -> i64, i64) -> i64
// ORDER1:   "test.use"(%3) : (i64) -> ()
// ORDER1: }

// ORDER2: builtin.module {
// ORDER2:   func.func @lifted_2(%0: i64) -> i64 {
// ORDER2:     func.return %0 : i64
// ORDER2:   }
// ORDER2:   %0 = func.constant @lifted_2 : (i64) -> i64
// ORDER2:   func.func @lifted_1(%1: i64) -> i64 {
// ORDER2:     func.return %1 : i64
// ORDER2:   }
// ORDER2:   %1 = "arith.constant"() <{value = 5}> : () -> i64
// ORDER2:   "test.use"(%1) : (i64) -> ()
// ORDER2: }

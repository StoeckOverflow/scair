// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam,monomorphize,erase-tlam,lower-tlam-to-func,canonicalize,cse | filecheck %s --check-prefix=ORDER1 -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,beta-reduce-tlam,erase-tlam,lower-tlam-to-func,canonicalize,cse | filecheck %s --check-prefix=ORDER2 -DFILE=%s

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
    %c = "arith.constant"() <{value = 5 : i64}> : () -> (i64)
    %r = "tlam.vapply"(%spec, %c) : (!tlam.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()
    %f2 = "tlam.vlambda"() ({
    ^bb0(%z: i64):
      "tlam.vreturn"(%z) : (i64) -> ()
    }) : () -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%f2) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
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

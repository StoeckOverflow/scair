// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p canonicalize,cse,monomorphize,erase-tlam,lower-tlam-to-func,canonicalize,cse | filecheck %s -DFILE=%s

// Pipeline smoke with cse/canonicalize around TLam lowering.
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
    %b = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)

    %c = "arith.constant"() <{value = 21 : i64}> : () -> (i64)
    %r = "tlam.vapply"(%a, %c) : (!tlam.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()

    %id2 = "tlam.vlambda"() ({
    ^bb0(%z: i64):
      "tlam.vreturn"(%z) : (i64) -> ()
    }) : () -> (!tlam.fun<i64, i64>)
    "tlam.treturn"(%id2) : (!tlam.fun<i64, i64>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i64, i64>>)
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

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

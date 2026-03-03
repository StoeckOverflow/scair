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

// ERASE: builtin.module {
// ERASE:   %0 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%1: i64):
// ERASE:     "tlam.vreturn"(%1) : (i64) -> ()
// ERASE:   }) : () -> !tlam.fun<i64, i64>
// ERASE:   %1 = "arith.constant"() <{value = 9}> : () -> i64
// ERASE:   %2 = "tlam.vapply"(%0, %1) : (!tlam.fun<i64, i64>, i64) -> i64
// ERASE:   "test.use"(%2) : (i64) -> ()
// ERASE:   %3 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%4: i64):
// ERASE:     "tlam.vreturn"(%4) : (i64) -> ()
// ERASE:   }) : () -> !tlam.fun<i64, i64>
// ERASE:   %4 = "tlam.vapply"(%3, %2) : (!tlam.fun<i64, i64>, i64) -> i64
// ERASE:   "test.use"(%4) : (i64) -> ()
// ERASE: }

// LOWER: builtin.module {
// LOWER:   func.func @lifted_2(%0: i64) -> i64 {
// LOWER:     func.return %0 : i64
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_2 : (i64) -> i64
// LOWER:   func.func @lifted_1(%1: i64) -> i64 {
// LOWER:     func.return %1 : i64
// LOWER:   }
// LOWER:   %1 = func.constant @lifted_1 : (i64) -> i64
// LOWER:   %2 = "arith.constant"() <{value = 9}> : () -> i64
// LOWER:   %3 = "func.call_indirect"(%1, %2) : ((i64) -> i64, i64) -> i64
// LOWER:   "test.use"(%3) : (i64) -> ()
// LOWER:   %4 = "func.call_indirect"(%0, %3) : ((i64) -> i64, i64) -> i64
// LOWER:   "test.use"(%4) : (i64) -> ()
// LOWER: }

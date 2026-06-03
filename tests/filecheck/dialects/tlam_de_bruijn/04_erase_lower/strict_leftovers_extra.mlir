// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn | filecheck %s --check-prefix=ERASE -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func | filecheck %s --check-prefix=LOWER -DFILE=%s

// EXTRA STRICTNESS: erase/lower should remove all TLam constructs in this shape too.
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
    %c = "arith.constant"() <{value = 9 : i64}> : () -> (i64)
    %r = "tlam_dbi.vapply"(%spec, %c) : (!tlam_dbi.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r) : (i64) -> ()

    %f2 = "tlam_dbi.vlambda"() ({
    ^bb0(%z: i64):
      "tlam_dbi.vreturn"(%z) : (i64) -> ()
    }) : () -> (!tlam_dbi.fun<i64, i64>)
    %r2 = "tlam_dbi.vapply"(%f2, %r) : (!tlam_dbi.fun<i64, i64>, i64) -> (i64)
    "test.use"(%r2) : (i64) -> ()

    "tlam_dbi.treturn"(%spec) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
  %top = "tlam_dbi.tapply"(%outer) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>) -> (!tlam_dbi.fun<i64, i64>)
  "test.use"(%top) : (!tlam_dbi.fun<i64, i64>) -> ()
}

// ERASE: builtin.module {
// ERASE:   %0 = "tlam_dbi.vlambda"() ({
// ERASE:   ^bb0(%1: i64):
// ERASE:     "tlam_dbi.vreturn"(%1) : (i64) -> ()
// ERASE:   }) : () -> !tlam_dbi.fun<i64, i64>
// ERASE:   %1 = "arith.constant"() <{value = 9}> : () -> i64
// ERASE:   %2 = "tlam_dbi.vapply"(%0, %1) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
// ERASE:   "test.use"(%2) : (i64) -> ()
// ERASE:   %3 = "tlam_dbi.vlambda"() ({
// ERASE:   ^bb0(%4: i64):
// ERASE:     "tlam_dbi.vreturn"(%4) : (i64) -> ()
// ERASE:   }) : () -> !tlam_dbi.fun<i64, i64>
// ERASE:   %4 = "tlam_dbi.vapply"(%3, %2) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
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

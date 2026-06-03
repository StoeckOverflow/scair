// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam-de-bruijn | filecheck %s --check-prefix=BETA -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn | filecheck %s --check-prefix=MONO -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn | filecheck %s --check-prefix=ERASE -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func | filecheck %s --check-prefix=LOWER -DFILE=%s

// Robustness: this corner shape should be handled consistently by all pass entrypoints.
builtin.module {
  %bad = "tlam_dbi.tlambda"() ({
    %f = "tlam_dbi.vlambda"() ({
    ^bb0(%x: i32, %y: i32):
      "tlam_dbi.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    "tlam_dbi.treturn"(%f) : (!tlam_dbi.fun<i32, i32>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>)
  %top = "tlam_dbi.tapply"(%bad) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>) -> (!tlam_dbi.fun<i32, i32>)
  "test.use"(%top) : (!tlam_dbi.fun<i32, i32>) -> ()
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam_dbi.tlambda"() ({
// VERIFY:     %1 = "tlam_dbi.vlambda"() ({
// VERIFY:     ^bb0(%2: i32, %3: i32):
// VERIFY:       "tlam_dbi.vreturn"(%2) : (i32) -> ()
// VERIFY:     }) : () -> !tlam_dbi.fun<i32, i32>
// VERIFY:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.fun<i32, i32>) -> ()
// VERIFY:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam_dbi.tlambda"() ({
// BETA:     %1 = "tlam_dbi.vlambda"() ({
// BETA:     ^bb0(%2: i32, %3: i32):
// BETA:       "tlam_dbi.vreturn"(%2) : (i32) -> ()
// BETA:     }) : () -> !tlam_dbi.fun<i32, i32>
// BETA:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.fun<i32, i32>) -> ()
// BETA:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam_dbi.tlambda"() ({
// MONO:     %1 = "tlam_dbi.vlambda"() ({
// MONO:     ^bb0(%2: i32, %3: i32):
// MONO:       "tlam_dbi.vreturn"(%2) : (i32) -> ()
// MONO:     }) : () -> !tlam_dbi.fun<i32, i32>
// MONO:     "tlam_dbi.treturn"(%1) : (!tlam_dbi.fun<i32, i32>) -> ()
// MONO:   }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>
// MONO: }
// ERASE: builtin.module {
// ERASE:   %0 = "tlam_dbi.vlambda"() ({
// ERASE:   ^bb0(%1: i32, %2: i32):
// ERASE:     "tlam_dbi.vreturn"(%1) : (i32) -> ()
// ERASE:   }) : () -> !tlam_dbi.fun<i32, i32>
// ERASE: }
// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i32, %1: i32) -> i32 {
// LOWER:     func.return %0 : i32
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i32) -> i32
// LOWER: }

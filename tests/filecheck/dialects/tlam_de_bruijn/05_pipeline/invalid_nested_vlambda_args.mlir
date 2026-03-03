// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam | filecheck %s --check-prefix=BETA -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize | filecheck %s --check-prefix=MONO -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam | filecheck %s --check-prefix=ERASE -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam,lower-tlam-to-func | filecheck %s --check-prefix=LOWER -DFILE=%s

// Robustness: this corner shape should be handled consistently by all pass entrypoints.
builtin.module {
  %bad = "tlam.tlambda"() ({
    %f = "tlam.vlambda"() ({
    ^bb0(%x: i32, %y: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    "tlam.treturn"(%f) : (!tlam.fun<i32, i32>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i32, i32>>)
}
// VERIFY: builtin.module {
// VERIFY:   %0 = "tlam.tlambda"() ({
// VERIFY:     %1 = "tlam.vlambda"() ({
// VERIFY:     ^bb0(%2: i32, %3: i32):
// VERIFY:       "tlam.vreturn"(%2) : (i32) -> ()
// VERIFY:     }) : () -> !tlam.fun<i32, i32>
// VERIFY:     "tlam.treturn"(%1) : (!tlam.fun<i32, i32>) -> ()
// VERIFY:   }) : () -> !tlam.forall<!tlam.fun<i32, i32>>
// VERIFY: }
// BETA: builtin.module {
// BETA:   %0 = "tlam.tlambda"() ({
// BETA:     %1 = "tlam.vlambda"() ({
// BETA:     ^bb0(%2: i32, %3: i32):
// BETA:       "tlam.vreturn"(%2) : (i32) -> ()
// BETA:     }) : () -> !tlam.fun<i32, i32>
// BETA:     "tlam.treturn"(%1) : (!tlam.fun<i32, i32>) -> ()
// BETA:   }) : () -> !tlam.forall<!tlam.fun<i32, i32>>
// BETA: }
// MONO: builtin.module {
// MONO:   %0 = "tlam.tlambda"() ({
// MONO:     %1 = "tlam.vlambda"() ({
// MONO:     ^bb0(%2: i32, %3: i32):
// MONO:       "tlam.vreturn"(%2) : (i32) -> ()
// MONO:     }) : () -> !tlam.fun<i32, i32>
// MONO:     "tlam.treturn"(%1) : (!tlam.fun<i32, i32>) -> ()
// MONO:   }) : () -> !tlam.forall<!tlam.fun<i32, i32>>
// MONO: }
// ERASE: builtin.module {
// ERASE:   %0 = "tlam.vlambda"() ({
// ERASE:   ^bb0(%1: i32, %2: i32):
// ERASE:     "tlam.vreturn"(%1) : (i32) -> ()
// ERASE:   }) : () -> !tlam.fun<i32, i32>
// ERASE: }
// LOWER: builtin.module {
// LOWER:   func.func @lifted_1(%0: i32, %1: i32) -> i32 {
// LOWER:     func.return %0 : i32
// LOWER:   }
// LOWER:   %0 = func.constant @lifted_1 : (i32) -> i32
// LOWER: }

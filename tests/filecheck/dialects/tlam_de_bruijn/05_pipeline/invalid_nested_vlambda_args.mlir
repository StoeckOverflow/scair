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
// VERIFY: builtin.module
// BETA: builtin.module
// MONO: builtin.module
// ERASE: builtin.module
// LOWER: builtin.module

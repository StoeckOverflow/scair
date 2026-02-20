// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam | filecheck %s --check-prefix=BETA -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize | filecheck %s --check-prefix=MONO -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam | filecheck %s --check-prefix=ERASE -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize,erase-tlam,lower-tlam-to-func | filecheck %s --check-prefix=LOWER -DFILE=%s

// INVALID: malformed tlambda terminator position should be diagnosed by all entrypoints.
builtin.module {
  %bad = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: i32):
      "tlam.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam.fun<i32, i32>)
    "tlam.treturn"(%id) : (!tlam.fun<i32, i32>) -> ()
    "test.op"() : () -> ()
  }) : () -> (!tlam.forall<!tlam.fun<i32, i32>>)
}
// VERIFY: tlambda: last op must be tlam.treturn
// BETA: tlambda: last op must be tlam.treturn
// MONO: tlambda: last op must be tlam.treturn
// ERASE: tlambda: last op must be tlam.treturn
// LOWER: tlambda: last op must be tlam.treturn

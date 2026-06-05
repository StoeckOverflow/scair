// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam-de-bruijn | filecheck %s --check-prefix=BETA -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn | filecheck %s --check-prefix=MONO -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn | filecheck %s --check-prefix=ERASE -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func | filecheck %s --check-prefix=LOWER -DFILE=%s

// INVALID: malformed tlambda terminator position should be diagnosed by all entrypoints.
builtin.module {
  %bad = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: i32):
      "tlam_dbi.vreturn"(%x) : (i32) -> ()
    }) : () -> (!tlam_dbi.fun<i32, i32>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<i32, i32>) -> ()
    "test.op"() : () -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i32, i32>>)
}
// VERIFY: tlambda: last op must be tlam_dbi.treturn, got 'test.op'
// BETA: tlambda: last op must be tlam_dbi.treturn, got 'test.op'
// MONO: tlambda: last op must be tlam_dbi.treturn, got 'test.op'
// ERASE: tlambda: last op must be tlam_dbi.treturn, got 'test.op'
// LOWER: tlambda: last op must be tlam_dbi.treturn, got 'test.op'

// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file | filecheck %s --check-prefix=VERIFY -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p beta-reduce-tlam-de-bruijn | filecheck %s --check-prefix=BETA -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn | filecheck %s --check-prefix=MONO -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn | filecheck %s --check-prefix=ERASE -DFILE=%s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func | filecheck %s --check-prefix=LOWER -DFILE=%s

// INVALID: vlambda block arg count must match unary tlam.fun input.
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
// VERIFY: vlambda: one block with one arg of input type required
// BETA: vlambda: one block with one arg of input type required
// MONO: vlambda: one block with one arg of input type required
// ERASE: vlambda: one block with one arg of input type required
// LOWER: vlambda: one block with one arg of input type required

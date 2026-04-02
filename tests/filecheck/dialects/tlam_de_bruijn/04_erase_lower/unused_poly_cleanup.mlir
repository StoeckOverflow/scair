// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics --split-input-file -p monomorphize-tlam-de-bruijn,erase-tlam-de-bruijn | filecheck %s --check-prefix=ERASE -DFILE=%s

// Erase with an unused polymorphic def and a used monomorphic path.
builtin.module {
  %unused = "tlam_dbi.tlambda"() ({
    %id0 = "tlam_dbi.vlambda"() ({
    ^bb0(%u: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%u) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id0) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

  %outer = "tlam_dbi.tlambda"() ({
    %poly = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
    %spec = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
    "tlam_dbi.treturn"(%spec) : (!tlam_dbi.fun<i64, i64>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<i64, i64>>)
}

// ERASE: builtin.module {
// ERASE:   %0 = "tlam_dbi.vlambda"() ({
// ERASE:   ^bb0(%1: !tlam_dbi.bvar<0>):
// ERASE:     "tlam_dbi.vreturn"(%1) : (!tlam_dbi.bvar<0>) -> ()
// ERASE:   }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
// ERASE:   %1 = "tlam_dbi.vlambda"() ({
// ERASE:   ^bb0(%2: i64):
// ERASE:     "tlam_dbi.vreturn"(%2) : (i64) -> ()
// ERASE:   }) : () -> !tlam_dbi.fun<i64, i64>
// ERASE: }

// RUN: ! scair-run %s 2>&1 | filecheck %s

builtin.module {
  %bad = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<1>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<1>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<1>, !tlam_dbi.bvar<1>>>)
}

// CHECK: debruijn-dbi: bvar<1> out of scope at depth=1

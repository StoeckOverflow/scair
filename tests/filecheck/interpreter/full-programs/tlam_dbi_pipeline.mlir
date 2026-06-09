// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize-tlam-de-bruijn,dce,erase-tlam-de-bruijn,lower-tlam-de-bruijn-to-func | scair-run | filecheck %s

builtin.module {
  %poly = "tlam_dbi.tlambda"() ({
    %id = "tlam_dbi.vlambda"() ({
    ^bb0(%x: !tlam_dbi.bvar<0>):
      "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
    }) : () -> (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>)
    "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
  }) : () -> (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)

  %spec = "tlam_dbi.tapply"(%poly) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> (!tlam_dbi.fun<i64, i64>)
  %c41 = "arith.constant"() <{value = 41 : i64}> : () -> i64
  %r = "tlam_dbi.vapply"(%spec, %c41) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
  "func.call"(%r) <{callee = @print}> : (i64) -> ()
}

// CHECK: Result: 41

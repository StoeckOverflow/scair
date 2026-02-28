// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize,erase-tlam,lower-tlam-to-func | scair-run | filecheck %s

builtin.module {
  %poly = "tlam.tlambda"() ({
    %id = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>)
    "tlam.treturn"(%id) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>) -> ()
  }) : () -> (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)

  %spec = "tlam.tapply"(%poly) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> (!tlam.fun<i64, i64>)
  %c41 = "arith.constant"() <{value = 41 : i64}> : () -> i64
  %r = "tlam.vapply"(%spec, %c41) : (!tlam.fun<i64, i64>, i64) -> i64
  "tlam.vreturn"(%r) : (i64) -> ()
}

// CHECK: Result: 41

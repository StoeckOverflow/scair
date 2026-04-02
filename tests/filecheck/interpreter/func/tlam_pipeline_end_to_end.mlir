// RUN: scair-opt %s --allow-unregistered-dialect -p monomorphize,erase-tlam,lower-tlam-to-func | scair-run | filecheck %s

builtin.module {
  %mk = "tlam.tlambda"() ({
  ^bb0(%T: !tlam.type):
    %id = "tlam.vlambda"() ({
    ^bb1(%x: !value<%T>):
      "tlam.vreturn"(%x) : (!value<%T>) -> ()
    }) : () -> !tlam.fun<!value<%T>, !value<%T>>
    "tlam.treturn"(%id) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
  }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

  %id_i32 = "tlam.tapply"(%mk) <{tyArg = i32}>
      : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>)
       -> !tlam.fun<i32, i32>

  %five = "arith.constant"() <{value = 5 : i32}> : () -> i32
  %res = "tlam.vapply"(%id_i32, %five) : (!tlam.fun<i32, i32>, i32) -> i32
  "func.call"(%res) <{"callee" = @print}> : (i32) -> ()
}

// CHECK: Result: 5

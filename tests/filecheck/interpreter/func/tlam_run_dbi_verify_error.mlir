// RUN: ! scair-run %s 2>&1 | filecheck %s

builtin.module {
  func.func @main() -> i32 {
    %f = "tlam.vlambda"() ({
    ^bb0(%x: !tlam.bvar<0>):
      "tlam.vreturn"(%x) : (!tlam.bvar<0>) -> ()
    }) : () -> !tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>

    %a = "arith.constant"() <{value = 5 : i32}> : () -> i32
    %r = "tlam.vapply"(%f, %a) : (!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>, i32) -> !tlam.bvar<0>
    func.return %r : i32
  }
}

// CHECK: debruijn: bvar<0> out of scope at depth=0
// CHECK-NOT: Unsupported operation when interpreting

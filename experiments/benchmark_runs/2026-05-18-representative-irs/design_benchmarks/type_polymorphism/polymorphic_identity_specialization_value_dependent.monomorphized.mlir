builtin.module {
  func.func @polymorphic_identity_specialization(%0: i32, %1: i64) -> i64 {
    %2 = "tlam.vlambda"() ({
    ^bb0(%3: i32):
      "tlam.vreturn"(%3) : (i32) -> ()
    }) : () -> !tlam.fun<i32, i32>
    %3 = "tlam.vlambda"() ({
    ^bb0(%4: i64):
      "tlam.vreturn"(%4) : (i64) -> ()
    }) : () -> !tlam.fun<i64, i64>
    %4 = "tlam.vapply"(%2, %0) : (!tlam.fun<i32, i32>, i32) -> i32
    %5 = "tlam.vapply"(%3, %1) : (!tlam.fun<i64, i64>, i64) -> i64
    %6 = "arith.extsi"(%4) : (i32) -> i64
    %7 = "arith.addi"(%6, %5) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %7 : i64
  }
}

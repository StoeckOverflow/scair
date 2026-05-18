builtin.module {
  func.func @polymorphic_identity_specialization(%0: i32, %1: i64) -> i64 {
    %2 = "tlam_dbi.vlambda"() ({
    ^bb0(%3: i32):
      "tlam_dbi.vreturn"(%3) : (i32) -> ()
    }) : () -> !tlam_dbi.fun<i32, i32>
    %3 = "tlam_dbi.vlambda"() ({
    ^bb0(%4: i64):
      "tlam_dbi.vreturn"(%4) : (i64) -> ()
    }) : () -> !tlam_dbi.fun<i64, i64>
    %4 = "tlam_dbi.vapply"(%2, %0) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
    %5 = "tlam_dbi.vapply"(%3, %1) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
    %6 = "arith.extsi"(%4) : (i32) -> i64
    %7 = "arith.addi"(%6, %5) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %7 : i64
  }
}

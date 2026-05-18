builtin.module {
  func.func @polymorphic_identity_specialization(%i32v: i32, %i64v: i64) -> i64 {
    %id = "tlam_dbi.tlambda"() ({
      %f = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
      "tlam_dbi.treturn"(%f) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>

    %id_i32 = "tlam_dbi.tapply"(%id) <{tyArg = i32}>
      : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
        -> !tlam_dbi.fun<i32, i32>
    %id_i64 = "tlam_dbi.tapply"(%id) <{tyArg = i64}>
      : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>)
        -> !tlam_dbi.fun<i64, i64>

    %r32 = "tlam_dbi.vapply"(%id_i32, %i32v) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
    %r64 = "tlam_dbi.vapply"(%id_i64, %i64v) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
    %r32_64 = "arith.extsi"(%r32) : (i32) -> i64
    %sum = "arith.addi"(%r32_64, %r64) : (i64, i64) -> i64
    func.return %sum : i64
  }
}

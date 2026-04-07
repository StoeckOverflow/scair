builtin.module {
  func.func @shared_polymorphic_projection_accumulator(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
    %sink = "tlam_dbi.tlambda"() ({
      %id = "tlam_dbi.vlambda"() ({
      ^bb0(%x: !tlam_dbi.bvar<0>):
        "tlam_dbi.vreturn"(%x) : (!tlam_dbi.bvar<0>) -> ()
      }) : () -> !tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>
      "tlam_dbi.treturn"(%id) : (!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>) -> ()
    }) : () -> !tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>

    %sink_i8 = "tlam_dbi.tapply"(%sink) <{tyArg = i8}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i8, i8>
    %sink_i16 = "tlam_dbi.tapply"(%sink) <{tyArg = i16}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i16, i16>
    %sink_i32 = "tlam_dbi.tapply"(%sink) <{tyArg = i32}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i32, i32>
    %sink_i64 = "tlam_dbi.tapply"(%sink) <{tyArg = i64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<i64, i64>
    %sink_f32 = "tlam_dbi.tapply"(%sink) <{tyArg = f32}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<f32, f32>
    %sink_f64 = "tlam_dbi.tapply"(%sink) <{tyArg = f64}> : (!tlam_dbi.forall<!tlam_dbi.fun<!tlam_dbi.bvar<0>, !tlam_dbi.bvar<0>>>) -> !tlam_dbi.fun<f64, f64>

    %i64_c7 = "arith.constant"() <{value = 7 : i64}> : () -> i64
    %i64_c2 = "arith.constant"() <{value = 2 : i64}> : () -> i64
    %i64_c9 = "arith.constant"() <{value = 9 : i64}> : () -> i64
    %i64_c5 = "arith.constant"() <{value = 5 : i64}> : () -> i64
    %f32_c2 = "arith.constant"() <{value = 2.0 : f32}> : () -> f32
    %f32_c1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
    %f64_c05 = "arith.constant"() <{value = 0.5 : f64}> : () -> f64
    %f64_c11 = "arith.constant"() <{value = 11.0 : f64}> : () -> f64

    %s_i8 = "tlam_dbi.vapply"(%sink_i8, %i8v) : (!tlam_dbi.fun<i8, i8>, i8) -> i8
    %s_i16 = "tlam_dbi.vapply"(%sink_i16, %i16v) : (!tlam_dbi.fun<i16, i16>, i16) -> i16
    %s_i32 = "tlam_dbi.vapply"(%sink_i32, %i32v) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
    %s_i64 = "tlam_dbi.vapply"(%sink_i64, %i64v) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
    %s_f32 = "tlam_dbi.vapply"(%sink_f32, %f32v) : (!tlam_dbi.fun<f32, f32>, f32) -> f32
    %s_f64 = "tlam_dbi.vapply"(%sink_f64, %f64v) : (!tlam_dbi.fun<f64, f64>, f64) -> f64

    %sx_i8 = "arith.extsi"(%s_i8) : (i8) -> i64
    %sx_i16 = "arith.extsi"(%s_i16) : (i16) -> i64
    %sx_i32 = "arith.extsi"(%s_i32) : (i32) -> i64
    %r_i8 = "arith.addi"(%sx_i8, %i64_c7) : (i64, i64) -> i64
    %r_i16 = "arith.muli"(%sx_i16, %i64_c2) : (i64, i64) -> i64
    %r_i32 = "arith.subi"(%sx_i32, %i64_c9) : (i64, i64) -> i64
    %r_i64 = "arith.addi"(%s_i64, %i64_c5) : (i64, i64) -> i64
    %m_f32 = "arith.mulf"(%s_f32, %f32_c2) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %a_f32 = "arith.addf"(%m_f32, %f32_c1) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %r_f32 = "arith.fptosi"(%a_f32) : (f32) -> i64
    %m_f64 = "arith.mulf"(%s_f64, %f64_c05) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %a_f64 = "arith.addf"(%m_f64, %f64_c11) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %r_f64 = "arith.fptosi"(%a_f64) : (f64) -> i64

    %a0 = "arith.addi"(%r_i8, %r_i16) : (i64, i64) -> i64
    %a1 = "arith.addi"(%r_i32, %r_i64) : (i64, i64) -> i64
    %a2 = "arith.addi"(%r_f32, %r_f64) : (i64, i64) -> i64
    %a3 = "arith.addi"(%a0, %a1) : (i64, i64) -> i64
    %sum = "arith.addi"(%a3, %a2) : (i64, i64) -> i64
    func.return %sum : i64
  }
}

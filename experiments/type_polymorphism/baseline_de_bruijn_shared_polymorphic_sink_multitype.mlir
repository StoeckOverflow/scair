// Benchmark purpose: de Bruijn baseline for compose-like fanout with one reusable polymorphic shell.
// Polymorphic combinator shape: conceptually forall T. (T -> T) -> (T -> T) -> T -> T; executable
// realization here keeps a first-order polymorphic sink `forall T. T -> T` live and then applies
// two typed worker steps per runtime value. This keeps the benchmark runnable in the current subset
// while still exposing the extra binder bookkeeping of the de Bruijn encoding.
// Scaling knobs: fixed type fanout over i8, i16, i32, i64, f32, f64; two worker steps per type.
// Expected comparison story: this should lower similarly to the value-dependent benchmark but with
// more binder bookkeeping in the source representation.
builtin.module {
  func.func @shared_polymorphic_sink_multitype(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
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

    %i8_c3 = "arith.constant"() <{value = 3 : i8}> : () -> i8
    %i8_c2 = "arith.constant"() <{value = 2 : i8}> : () -> i8
    %i16_c5 = "arith.constant"() <{value = 5 : i16}> : () -> i16
    %i16_c11 = "arith.constant"() <{value = 11 : i16}> : () -> i16
    %i32_c3 = "arith.constant"() <{value = 3 : i32}> : () -> i32
    %i32_c7 = "arith.constant"() <{value = 7 : i32}> : () -> i32
    %i64_c13 = "arith.constant"() <{value = 13 : i64}> : () -> i64
    %i64_c2 = "arith.constant"() <{value = 2 : i64}> : () -> i64
    %f32_c15 = "arith.constant"() <{value = 1.5 : f32}> : () -> f32
    %f32_c225 = "arith.constant"() <{value = 2.25 : f32}> : () -> f32
    %f64_c35 = "arith.constant"() <{value = 3.5 : f64}> : () -> f64
    %f64_c05 = "arith.constant"() <{value = 0.5 : f64}> : () -> f64

    %s_i8 = "tlam_dbi.vapply"(%sink_i8, %i8v) : (!tlam_dbi.fun<i8, i8>, i8) -> i8
    %f_i8 = "arith.addi"(%s_i8, %i8_c3) : (i8, i8) -> i8
    %r_i8 = "arith.muli"(%f_i8, %i8_c2) : (i8, i8) -> i8

    %s_i16 = "tlam_dbi.vapply"(%sink_i16, %i16v) : (!tlam_dbi.fun<i16, i16>, i16) -> i16
    %f_i16 = "arith.addi"(%s_i16, %i16_c5) : (i16, i16) -> i16
    %r_i16 = "arith.addi"(%f_i16, %i16_c11) : (i16, i16) -> i16

    %s_i32 = "tlam_dbi.vapply"(%sink_i32, %i32v) : (!tlam_dbi.fun<i32, i32>, i32) -> i32
    %f_i32 = "arith.muli"(%s_i32, %i32_c3) : (i32, i32) -> i32
    %r_i32 = "arith.subi"(%f_i32, %i32_c7) : (i32, i32) -> i32

    %s_i64 = "tlam_dbi.vapply"(%sink_i64, %i64v) : (!tlam_dbi.fun<i64, i64>, i64) -> i64
    %f_i64 = "arith.addi"(%s_i64, %i64_c13) : (i64, i64) -> i64
    %r_i64 = "arith.muli"(%f_i64, %i64_c2) : (i64, i64) -> i64

    %s_f32 = "tlam_dbi.vapply"(%sink_f32, %f32v) : (!tlam_dbi.fun<f32, f32>, f32) -> f32
    %f_f32 = "arith.mulf"(%s_f32, %f32_c15) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %r_f32 = "arith.addf"(%f_f32, %f32_c225) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32

    %s_f64 = "tlam_dbi.vapply"(%sink_f64, %f64v) : (!tlam_dbi.fun<f64, f64>, f64) -> f64
    %f_f64 = "arith.addf"(%s_f64, %f64_c35) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %r_f64 = "arith.mulf"(%f_f64, %f64_c05) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64

    %sum_i8 = "arith.extsi"(%r_i8) : (i8) -> i64
    %sum_i16 = "arith.extsi"(%r_i16) : (i16) -> i64
    %sum_i32 = "arith.extsi"(%r_i32) : (i32) -> i64
    %sum_f32 = "arith.fptosi"(%r_f32) : (f32) -> i64
    %sum_f64 = "arith.fptosi"(%r_f64) : (f64) -> i64

    %a0 = "arith.addi"(%sum_i8, %sum_i16) : (i64, i64) -> i64
    %a1 = "arith.addi"(%sum_i32, %r_i64) : (i64, i64) -> i64
    %a2 = "arith.addi"(%sum_f32, %sum_f64) : (i64, i64) -> i64
    %a3 = "arith.addi"(%a0, %a1) : (i64, i64) -> i64
    %sum = "arith.addi"(%a3, %a2) : (i64, i64) -> i64
    func.return %sum : i64
  }
}

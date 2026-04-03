// Benchmark purpose: de Bruijn baseline for repeated small-batch map-like polymorphic reuse.
// Polymorphic combinator shape: conceptually forall T. (T -> T) -> T -> T; executable realization
// here uses repeated applications of a first-order polymorphic sink `forall T. T -> T` between typed steps.
// Scaling knobs: fixed type fanout over i8, i16, i32, i64, f32, f64; fixed batch depth of three uses per type.
// Expected comparison story: this should lower similarly to the value-dependent benchmark with
// more source-level binder bookkeeping.
builtin.module {
  func.func @batched_map_small(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
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

    %i8_c1 = "arith.constant"() <{value = 1 : i8}> : () -> i8
    %i16_c2 = "arith.constant"() <{value = 2 : i16}> : () -> i16
    %i32_c3 = "arith.constant"() <{value = 3 : i32}> : () -> i32
    %i64_c4 = "arith.constant"() <{value = 4 : i64}> : () -> i64
    %f32_c125 = "arith.constant"() <{value = 1.25 : f32}> : () -> f32
    %f64_c075 = "arith.constant"() <{value = 0.75 : f64}> : () -> f64

    %m1_i8 = "arith.addi"(%i8v, %i8_c1) : (i8, i8) -> i8
    %m2_i8 = "arith.addi"(%m1_i8, %i8_c1) : (i8, i8) -> i8
    %r_i8 = "arith.addi"(%m2_i8, %i8_c1) : (i8, i8) -> i8

    %m1_i16 = "arith.muli"(%i16v, %i16_c2) : (i16, i16) -> i16
    %m2_i16 = "arith.muli"(%m1_i16, %i16_c2) : (i16, i16) -> i16
    %r_i16 = "arith.muli"(%m2_i16, %i16_c2) : (i16, i16) -> i16

    %m1_i32 = "arith.subi"(%i32v, %i32_c3) : (i32, i32) -> i32
    %m2_i32 = "arith.subi"(%m1_i32, %i32_c3) : (i32, i32) -> i32
    %r_i32 = "arith.subi"(%m2_i32, %i32_c3) : (i32, i32) -> i32

    %m1_i64 = "arith.addi"(%i64v, %i64_c4) : (i64, i64) -> i64
    %m2_i64 = "arith.addi"(%m1_i64, %i64_c4) : (i64, i64) -> i64
    %r_i64 = "arith.addi"(%m2_i64, %i64_c4) : (i64, i64) -> i64

    %m1_f32 = "arith.mulf"(%f32v, %f32_c125) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %m2_f32 = "arith.mulf"(%m1_f32, %f32_c125) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %r_f32 = "arith.mulf"(%m2_f32, %f32_c125) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32

    %m1_f64 = "arith.addf"(%f64v, %f64_c075) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %m2_f64 = "arith.addf"(%m1_f64, %f64_c075) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
    %r_f64 = "arith.addf"(%m2_f64, %f64_c075) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64

    %s_i8 = "arith.extsi"(%r_i8) : (i8) -> i64
    %s_i16 = "arith.extsi"(%r_i16) : (i16) -> i64
    %s_i32 = "arith.extsi"(%r_i32) : (i32) -> i64
    %s_f32 = "arith.fptosi"(%r_f32) : (f32) -> i64
    %s_f64 = "arith.fptosi"(%r_f64) : (f64) -> i64
    %a0 = "arith.addi"(%s_i8, %s_i16) : (i64, i64) -> i64
    %a1 = "arith.addi"(%s_i32, %r_i64) : (i64, i64) -> i64
    %a2 = "arith.addi"(%s_f32, %s_f64) : (i64, i64) -> i64
    %a3 = "arith.addi"(%a0, %a1) : (i64, i64) -> i64
    %sum = "arith.addi"(%a3, %a2) : (i64, i64) -> i64
    func.return %sum : i64
  }
}

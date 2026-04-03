// Benchmark purpose: design benchmark for repeated small-batch map-like polymorphic reuse.
// Polymorphic combinator shape: conceptually forall T. (T -> T) -> T -> T; executable realization
// here uses repeated applications of a first-order polymorphic sink `forall T. T -> T` between typed steps.
// Scaling knobs: fixed type fanout over i8, i16, i32, i64, f32, f64; fixed batch depth of three uses per type.
// Expected comparison story: MLIR duplicates sink/map wrappers per type, while ScaIR shares one
// polymorphic shell and still carries the same typed step chain.
builtin.module {
  func.func @batched_map_small(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
    %sink = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %id = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        "tlam.vreturn"(%x) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%id) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

    %sink_i8 = "tlam.tapply"(%sink) <{tyArg = i8}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i8, i8>
    %sink_i16 = "tlam.tapply"(%sink) <{tyArg = i16}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i16, i16>
    %sink_i32 = "tlam.tapply"(%sink) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
    %sink_i64 = "tlam.tapply"(%sink) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
    %sink_f32 = "tlam.tapply"(%sink) <{tyArg = f32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f32, f32>
    %sink_f64 = "tlam.tapply"(%sink) <{tyArg = f64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f64, f64>

    %i8_c1 = "arith.constant"() <{value = 1 : i8}> : () -> i8
    %i16_c2 = "arith.constant"() <{value = 2 : i16}> : () -> i16
    %i32_c3 = "arith.constant"() <{value = 3 : i32}> : () -> i32
    %i64_c4 = "arith.constant"() <{value = 4 : i64}> : () -> i64
    %f32_c125 = "arith.constant"() <{value = 1.25 : f32}> : () -> f32
    %f64_c075 = "arith.constant"() <{value = 0.75 : f64}> : () -> f64

    %s1_i8 = "tlam.vapply"(%sink_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8
    %m1_i8 = "arith.addi"(%s1_i8, %i8_c1) : (i8, i8) -> i8
    %m2_i8 = "arith.addi"(%m1_i8, %i8_c1) : (i8, i8) -> i8
    %r_i8 = "arith.addi"(%m2_i8, %i8_c1) : (i8, i8) -> i8

    %s1_i16 = "tlam.vapply"(%sink_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16
    %m1_i16 = "arith.muli"(%s1_i16, %i16_c2) : (i16, i16) -> i16
    %m2_i16 = "arith.muli"(%m1_i16, %i16_c2) : (i16, i16) -> i16
    %r_i16 = "arith.muli"(%m2_i16, %i16_c2) : (i16, i16) -> i16

    %s1_i32 = "tlam.vapply"(%sink_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %m1_i32 = "arith.subi"(%s1_i32, %i32_c3) : (i32, i32) -> i32
    %m2_i32 = "arith.subi"(%m1_i32, %i32_c3) : (i32, i32) -> i32
    %r_i32 = "arith.subi"(%m2_i32, %i32_c3) : (i32, i32) -> i32

    %s1_i64 = "tlam.vapply"(%sink_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %m1_i64 = "arith.addi"(%s1_i64, %i64_c4) : (i64, i64) -> i64
    %m2_i64 = "arith.addi"(%m1_i64, %i64_c4) : (i64, i64) -> i64
    %r_i64 = "arith.addi"(%m2_i64, %i64_c4) : (i64, i64) -> i64

    %s1_f32 = "tlam.vapply"(%sink_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32
    %m1_f32 = "arith.mulf"(%s1_f32, %f32_c125) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %m2_f32 = "arith.mulf"(%m1_f32, %f32_c125) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %r_f32 = "arith.mulf"(%m2_f32, %f32_c125) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32

    %s1_f64 = "tlam.vapply"(%sink_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64
    %m1_f64 = "arith.addf"(%s1_f64, %f64_c075) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
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

// Benchmark purpose: design benchmark for accumulator-style polymorphism with typed scorers.
// Polymorphic combinator shape: conceptually forall T. (T -> i64) -> T -> i64; executable
// realization here uses a first-order polymorphic sink `forall T. T -> T` before typed scoring.
// Scaling knobs: fixed type fanout over i8, i16, i32, i64, f32, f64; one scorer use per type.
// Expected comparison story: MLIR duplicates sink and scorer wrappers per type, while the ScaIR
// encodings share one polymorphic shell and differ mainly in source-level bookkeeping.
builtin.module {
  func.func @higher_order_accumulator(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
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

    %i64_c7 = "arith.constant"() <{value = 7 : i64}> : () -> i64
    %i64_c2 = "arith.constant"() <{value = 2 : i64}> : () -> i64
    %i64_c9 = "arith.constant"() <{value = 9 : i64}> : () -> i64
    %i64_c5 = "arith.constant"() <{value = 5 : i64}> : () -> i64
    %f32_c2 = "arith.constant"() <{value = 2.0 : f32}> : () -> f32
    %f32_c1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
    %f64_c05 = "arith.constant"() <{value = 0.5 : f64}> : () -> f64
    %f64_c11 = "arith.constant"() <{value = 11.0 : f64}> : () -> f64

    %sx_i8 = "arith.extsi"(%i8v) : (i8) -> i64
    %sx_i16 = "arith.extsi"(%i16v) : (i16) -> i64
    %sx_i32 = "arith.extsi"(%i32v) : (i32) -> i64
    %r_i8 = "arith.addi"(%sx_i8, %i64_c7) : (i64, i64) -> i64
    %r_i16 = "arith.muli"(%sx_i16, %i64_c2) : (i64, i64) -> i64
    %r_i32 = "arith.subi"(%sx_i32, %i64_c9) : (i64, i64) -> i64
    %r_i64 = "arith.addi"(%i64v, %i64_c5) : (i64, i64) -> i64
    %m_f32 = "arith.mulf"(%f32v, %f32_c2) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %a_f32 = "arith.addf"(%m_f32, %f32_c1) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %r_f32 = "arith.fptosi"(%a_f32) : (f32) -> i64
    %m_f64 = "arith.mulf"(%f64v, %f64_c05) <{fastmath = #arith.fastmath<none>}> : (f64, f64) -> f64
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

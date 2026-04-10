builtin.module {
  func.func @shared_polymorphic_identity_multitype(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
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

    %r_i8 = "tlam.vapply"(%sink_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8
    %r_i16 = "tlam.vapply"(%sink_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16
    %r_i32 = "tlam.vapply"(%sink_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %r_i64 = "tlam.vapply"(%sink_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %r_f32 = "tlam.vapply"(%sink_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32
    %r_f64 = "tlam.vapply"(%sink_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64

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

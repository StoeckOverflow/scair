builtin.module {
  func.func @shared_polymorphic_kernel_bank_multitype(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
    %int_k1 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %y = "arith.addi"(%x, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %int_k2 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %y = "arith.muli"(%x, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %int_k3 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %d = "arith.addi"(%x, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.addi"(%d, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %int_k4 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %sq = "arith.muli"(%x, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.addi"(%sq, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %int_k5 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %d = "arith.addi"(%x, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.muli"(%d, %d) : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %int_k6 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %sq = "arith.muli"(%x, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.muli"(%sq, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %int_k7 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %sq = "arith.muli"(%x, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.addi"(%sq, %sq) : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %int_k8 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %d = "arith.addi"(%x, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        %t = "arith.addi"(%d, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.muli"(%t, %x) : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

    %flt_k1 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %y = "arith.addf"(%x, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %flt_k2 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %y = "arith.mulf"(%x, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %flt_k3 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %d = "arith.addf"(%x, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.addf"(%d, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %flt_k4 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %sq = "arith.mulf"(%x, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.addf"(%sq, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %flt_k5 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %d = "arith.addf"(%x, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.mulf"(%d, %d) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %flt_k6 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %sq = "arith.mulf"(%x, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.mulf"(%sq, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %flt_k7 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %sq = "arith.mulf"(%x, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.addf"(%sq, %sq) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>
    %flt_k8 = "tlam.tlambda"() ({
    ^bb0(%T: !tlam.type):
      %f = "tlam.vlambda"() ({
      ^bb1(%x: !value<%T>):
        %d = "arith.addf"(%x, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        %t = "arith.addf"(%d, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        %y = "arith.mulf"(%t, %x) <{fastmath = #arith.fastmath<none>}> : (!value<%T>, !value<%T>) -> !value<%T>
        "tlam.vreturn"(%y) : (!value<%T>) -> ()
      }) : () -> !tlam.fun<!value<%T>, !value<%T>>
      "tlam.treturn"(%f) : (!tlam.fun<!value<%T>, !value<%T>>) -> ()
    }) : () -> !tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>

    %int1_i8 = "tlam.tapply"(%int_k1) <{tyArg = i8}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i8, i8>
    %int2_i8 = "tlam.tapply"(%int_k2) <{tyArg = i8}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i8, i8>
    %int3_i8 = "tlam.tapply"(%int_k3) <{tyArg = i8}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i8, i8>
    %int4_i8 = "tlam.tapply"(%int_k4) <{tyArg = i8}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i8, i8>
    %int5_i8 = "tlam.tapply"(%int_k5) <{tyArg = i8}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i8, i8>
    %int6_i8 = "tlam.tapply"(%int_k6) <{tyArg = i8}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i8, i8>
    %int7_i8 = "tlam.tapply"(%int_k7) <{tyArg = i8}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i8, i8>
    %int8_i8 = "tlam.tapply"(%int_k8) <{tyArg = i8}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i8, i8>
    %r1_i8 = "tlam.vapply"(%int1_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8
    %r2_i8 = "tlam.vapply"(%int2_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8
    %r3_i8 = "tlam.vapply"(%int3_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8
    %r4_i8 = "tlam.vapply"(%int4_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8
    %r5_i8 = "tlam.vapply"(%int5_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8
    %r6_i8 = "tlam.vapply"(%int6_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8
    %r7_i8 = "tlam.vapply"(%int7_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8
    %r8_i8 = "tlam.vapply"(%int8_i8, %i8v) : (!tlam.fun<i8, i8>, i8) -> i8

    %int1_i16 = "tlam.tapply"(%int_k1) <{tyArg = i16}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i16, i16>
    %int2_i16 = "tlam.tapply"(%int_k2) <{tyArg = i16}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i16, i16>
    %int3_i16 = "tlam.tapply"(%int_k3) <{tyArg = i16}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i16, i16>
    %int4_i16 = "tlam.tapply"(%int_k4) <{tyArg = i16}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i16, i16>
    %int5_i16 = "tlam.tapply"(%int_k5) <{tyArg = i16}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i16, i16>
    %int6_i16 = "tlam.tapply"(%int_k6) <{tyArg = i16}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i16, i16>
    %int7_i16 = "tlam.tapply"(%int_k7) <{tyArg = i16}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i16, i16>
    %int8_i16 = "tlam.tapply"(%int_k8) <{tyArg = i16}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i16, i16>
    %r1_i16 = "tlam.vapply"(%int1_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16
    %r2_i16 = "tlam.vapply"(%int2_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16
    %r3_i16 = "tlam.vapply"(%int3_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16
    %r4_i16 = "tlam.vapply"(%int4_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16
    %r5_i16 = "tlam.vapply"(%int5_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16
    %r6_i16 = "tlam.vapply"(%int6_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16
    %r7_i16 = "tlam.vapply"(%int7_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16
    %r8_i16 = "tlam.vapply"(%int8_i16, %i16v) : (!tlam.fun<i16, i16>, i16) -> i16

    %int1_i32 = "tlam.tapply"(%int_k1) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
    %int2_i32 = "tlam.tapply"(%int_k2) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
    %int3_i32 = "tlam.tapply"(%int_k3) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
    %int4_i32 = "tlam.tapply"(%int_k4) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
    %int5_i32 = "tlam.tapply"(%int_k5) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
    %int6_i32 = "tlam.tapply"(%int_k6) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
    %int7_i32 = "tlam.tapply"(%int_k7) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
    %int8_i32 = "tlam.tapply"(%int_k8) <{tyArg = i32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i32, i32>
    %r1_i32 = "tlam.vapply"(%int1_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %r2_i32 = "tlam.vapply"(%int2_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %r3_i32 = "tlam.vapply"(%int3_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %r4_i32 = "tlam.vapply"(%int4_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %r5_i32 = "tlam.vapply"(%int5_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %r6_i32 = "tlam.vapply"(%int6_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %r7_i32 = "tlam.vapply"(%int7_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32
    %r8_i32 = "tlam.vapply"(%int8_i32, %i32v) : (!tlam.fun<i32, i32>, i32) -> i32

    %int1_i64 = "tlam.tapply"(%int_k1) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
    %int2_i64 = "tlam.tapply"(%int_k2) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
    %int3_i64 = "tlam.tapply"(%int_k3) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
    %int4_i64 = "tlam.tapply"(%int_k4) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
    %int5_i64 = "tlam.tapply"(%int_k5) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
    %int6_i64 = "tlam.tapply"(%int_k6) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
    %int7_i64 = "tlam.tapply"(%int_k7) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
    %int8_i64 = "tlam.tapply"(%int_k8) <{tyArg = i64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<i64, i64>
    %r1_i64 = "tlam.vapply"(%int1_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %r2_i64 = "tlam.vapply"(%int2_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %r3_i64 = "tlam.vapply"(%int3_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %r4_i64 = "tlam.vapply"(%int4_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %r5_i64 = "tlam.vapply"(%int5_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %r6_i64 = "tlam.vapply"(%int6_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %r7_i64 = "tlam.vapply"(%int7_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64
    %r8_i64 = "tlam.vapply"(%int8_i64, %i64v) : (!tlam.fun<i64, i64>, i64) -> i64

    %flt1_f32 = "tlam.tapply"(%flt_k1) <{tyArg = f32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f32, f32>
    %flt2_f32 = "tlam.tapply"(%flt_k2) <{tyArg = f32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f32, f32>
    %flt3_f32 = "tlam.tapply"(%flt_k3) <{tyArg = f32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f32, f32>
    %flt4_f32 = "tlam.tapply"(%flt_k4) <{tyArg = f32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f32, f32>
    %flt5_f32 = "tlam.tapply"(%flt_k5) <{tyArg = f32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f32, f32>
    %flt6_f32 = "tlam.tapply"(%flt_k6) <{tyArg = f32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f32, f32>
    %flt7_f32 = "tlam.tapply"(%flt_k7) <{tyArg = f32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f32, f32>
    %flt8_f32 = "tlam.tapply"(%flt_k8) <{tyArg = f32}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f32, f32>
    %rf1_f32 = "tlam.vapply"(%flt1_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32
    %rf2_f32 = "tlam.vapply"(%flt2_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32
    %rf3_f32 = "tlam.vapply"(%flt3_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32
    %rf4_f32 = "tlam.vapply"(%flt4_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32
    %rf5_f32 = "tlam.vapply"(%flt5_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32
    %rf6_f32 = "tlam.vapply"(%flt6_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32
    %rf7_f32 = "tlam.vapply"(%flt7_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32
    %rf8_f32 = "tlam.vapply"(%flt8_f32, %f32v) : (!tlam.fun<f32, f32>, f32) -> f32

    %flt1_f64 = "tlam.tapply"(%flt_k1) <{tyArg = f64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f64, f64>
    %flt2_f64 = "tlam.tapply"(%flt_k2) <{tyArg = f64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f64, f64>
    %flt3_f64 = "tlam.tapply"(%flt_k3) <{tyArg = f64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f64, f64>
    %flt4_f64 = "tlam.tapply"(%flt_k4) <{tyArg = f64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f64, f64>
    %flt5_f64 = "tlam.tapply"(%flt_k5) <{tyArg = f64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f64, f64>
    %flt6_f64 = "tlam.tapply"(%flt_k6) <{tyArg = f64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f64, f64>
    %flt7_f64 = "tlam.tapply"(%flt_k7) <{tyArg = f64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f64, f64>
    %flt8_f64 = "tlam.tapply"(%flt_k8) <{tyArg = f64}> : (!tlam.forall<!tlam.fun<!tlam.bvar<0>, !tlam.bvar<0>>>) -> !tlam.fun<f64, f64>
    %rf1_f64 = "tlam.vapply"(%flt1_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64
    %rf2_f64 = "tlam.vapply"(%flt2_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64
    %rf3_f64 = "tlam.vapply"(%flt3_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64
    %rf4_f64 = "tlam.vapply"(%flt4_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64
    %rf5_f64 = "tlam.vapply"(%flt5_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64
    %rf6_f64 = "tlam.vapply"(%flt6_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64
    %rf7_f64 = "tlam.vapply"(%flt7_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64
    %rf8_f64 = "tlam.vapply"(%flt8_f64, %f64v) : (!tlam.fun<f64, f64>, f64) -> f64

    %si1_i8 = "arith.extsi"(%r1_i8) : (i8) -> i64
    %si2_i8 = "arith.extsi"(%r2_i8) : (i8) -> i64
    %si3_i8 = "arith.extsi"(%r3_i8) : (i8) -> i64
    %si4_i8 = "arith.extsi"(%r4_i8) : (i8) -> i64
    %si5_i8 = "arith.extsi"(%r5_i8) : (i8) -> i64
    %si6_i8 = "arith.extsi"(%r6_i8) : (i8) -> i64
    %si7_i8 = "arith.extsi"(%r7_i8) : (i8) -> i64
    %si8_i8 = "arith.extsi"(%r8_i8) : (i8) -> i64
    %si1_i16 = "arith.extsi"(%r1_i16) : (i16) -> i64
    %si2_i16 = "arith.extsi"(%r2_i16) : (i16) -> i64
    %si3_i16 = "arith.extsi"(%r3_i16) : (i16) -> i64
    %si4_i16 = "arith.extsi"(%r4_i16) : (i16) -> i64
    %si5_i16 = "arith.extsi"(%r5_i16) : (i16) -> i64
    %si6_i16 = "arith.extsi"(%r6_i16) : (i16) -> i64
    %si7_i16 = "arith.extsi"(%r7_i16) : (i16) -> i64
    %si8_i16 = "arith.extsi"(%r8_i16) : (i16) -> i64
    %si1_i32 = "arith.extsi"(%r1_i32) : (i32) -> i64
    %si2_i32 = "arith.extsi"(%r2_i32) : (i32) -> i64
    %si3_i32 = "arith.extsi"(%r3_i32) : (i32) -> i64
    %si4_i32 = "arith.extsi"(%r4_i32) : (i32) -> i64
    %si5_i32 = "arith.extsi"(%r5_i32) : (i32) -> i64
    %si6_i32 = "arith.extsi"(%r6_i32) : (i32) -> i64
    %si7_i32 = "arith.extsi"(%r7_i32) : (i32) -> i64
    %si8_i32 = "arith.extsi"(%r8_i32) : (i32) -> i64
    %sf1_f32 = "arith.fptosi"(%rf1_f32) : (f32) -> i64
    %sf2_f32 = "arith.fptosi"(%rf2_f32) : (f32) -> i64
    %sf3_f32 = "arith.fptosi"(%rf3_f32) : (f32) -> i64
    %sf4_f32 = "arith.fptosi"(%rf4_f32) : (f32) -> i64
    %sf5_f32 = "arith.fptosi"(%rf5_f32) : (f32) -> i64
    %sf6_f32 = "arith.fptosi"(%rf6_f32) : (f32) -> i64
    %sf7_f32 = "arith.fptosi"(%rf7_f32) : (f32) -> i64
    %sf8_f32 = "arith.fptosi"(%rf8_f32) : (f32) -> i64
    %sf1_f64 = "arith.fptosi"(%rf1_f64) : (f64) -> i64
    %sf2_f64 = "arith.fptosi"(%rf2_f64) : (f64) -> i64
    %sf3_f64 = "arith.fptosi"(%rf3_f64) : (f64) -> i64
    %sf4_f64 = "arith.fptosi"(%rf4_f64) : (f64) -> i64
    %sf5_f64 = "arith.fptosi"(%rf5_f64) : (f64) -> i64
    %sf6_f64 = "arith.fptosi"(%rf6_f64) : (f64) -> i64
    %sf7_f64 = "arith.fptosi"(%rf7_f64) : (f64) -> i64
    %sf8_f64 = "arith.fptosi"(%rf8_f64) : (f64) -> i64

    %a0 = "arith.addi"(%si1_i8, %si2_i8) : (i64, i64) -> i64
    %a1 = "arith.addi"(%a0, %si3_i8) : (i64, i64) -> i64
    %a2 = "arith.addi"(%a1, %si4_i8) : (i64, i64) -> i64
    %a3 = "arith.addi"(%a2, %si5_i8) : (i64, i64) -> i64
    %a4 = "arith.addi"(%a3, %si6_i8) : (i64, i64) -> i64
    %a5 = "arith.addi"(%a4, %si7_i8) : (i64, i64) -> i64
    %a6 = "arith.addi"(%a5, %si8_i8) : (i64, i64) -> i64
    %a7 = "arith.addi"(%a6, %si1_i16) : (i64, i64) -> i64
    %a8 = "arith.addi"(%a7, %si2_i16) : (i64, i64) -> i64
    %a9 = "arith.addi"(%a8, %si3_i16) : (i64, i64) -> i64
    %a10 = "arith.addi"(%a9, %si4_i16) : (i64, i64) -> i64
    %a11 = "arith.addi"(%a10, %si5_i16) : (i64, i64) -> i64
    %a12 = "arith.addi"(%a11, %si6_i16) : (i64, i64) -> i64
    %a13 = "arith.addi"(%a12, %si7_i16) : (i64, i64) -> i64
    %a14 = "arith.addi"(%a13, %si8_i16) : (i64, i64) -> i64
    %a15 = "arith.addi"(%a14, %si1_i32) : (i64, i64) -> i64
    %a16 = "arith.addi"(%a15, %si2_i32) : (i64, i64) -> i64
    %a17 = "arith.addi"(%a16, %si3_i32) : (i64, i64) -> i64
    %a18 = "arith.addi"(%a17, %si4_i32) : (i64, i64) -> i64
    %a19 = "arith.addi"(%a18, %si5_i32) : (i64, i64) -> i64
    %a20 = "arith.addi"(%a19, %si6_i32) : (i64, i64) -> i64
    %a21 = "arith.addi"(%a20, %si7_i32) : (i64, i64) -> i64
    %a22 = "arith.addi"(%a21, %si8_i32) : (i64, i64) -> i64
    %a23 = "arith.addi"(%a22, %r1_i64) : (i64, i64) -> i64
    %a24 = "arith.addi"(%a23, %r2_i64) : (i64, i64) -> i64
    %a25 = "arith.addi"(%a24, %r3_i64) : (i64, i64) -> i64
    %a26 = "arith.addi"(%a25, %r4_i64) : (i64, i64) -> i64
    %a27 = "arith.addi"(%a26, %r5_i64) : (i64, i64) -> i64
    %a28 = "arith.addi"(%a27, %r6_i64) : (i64, i64) -> i64
    %a29 = "arith.addi"(%a28, %r7_i64) : (i64, i64) -> i64
    %a30 = "arith.addi"(%a29, %r8_i64) : (i64, i64) -> i64
    %a31 = "arith.addi"(%a30, %sf1_f32) : (i64, i64) -> i64
    %a32 = "arith.addi"(%a31, %sf2_f32) : (i64, i64) -> i64
    %a33 = "arith.addi"(%a32, %sf3_f32) : (i64, i64) -> i64
    %a34 = "arith.addi"(%a33, %sf4_f32) : (i64, i64) -> i64
    %a35 = "arith.addi"(%a34, %sf5_f32) : (i64, i64) -> i64
    %a36 = "arith.addi"(%a35, %sf6_f32) : (i64, i64) -> i64
    %a37 = "arith.addi"(%a36, %sf7_f32) : (i64, i64) -> i64
    %a38 = "arith.addi"(%a37, %sf8_f32) : (i64, i64) -> i64
    %a39 = "arith.addi"(%a38, %sf1_f64) : (i64, i64) -> i64
    %a40 = "arith.addi"(%a39, %sf2_f64) : (i64, i64) -> i64
    %a41 = "arith.addi"(%a40, %sf3_f64) : (i64, i64) -> i64
    %a42 = "arith.addi"(%a41, %sf4_f64) : (i64, i64) -> i64
    %a43 = "arith.addi"(%a42, %sf5_f64) : (i64, i64) -> i64
    %a44 = "arith.addi"(%a43, %sf6_f64) : (i64, i64) -> i64
    %a45 = "arith.addi"(%a44, %sf7_f64) : (i64, i64) -> i64
    %sum = "arith.addi"(%a45, %sf8_f64) : (i64, i64) -> i64
    func.return %sum : i64
  }
}

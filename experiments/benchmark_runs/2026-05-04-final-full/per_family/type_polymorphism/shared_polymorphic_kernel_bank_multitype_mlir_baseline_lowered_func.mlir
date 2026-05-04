// Monomorphic baseline for the kernel-bank benchmark.
// Each kernel is duplicated once per concrete type, making the source-level
// function explosion explicit.
module {
  func.func @int_k1_i8(%x: i8) -> i8 {
    %y = arith.addi %x, %x : i8
    return %y : i8
  }

  func.func @int_k2_i8(%x: i8) -> i8 {
    %y = arith.muli %x, %x : i8
    return %y : i8
  }

  func.func @int_k3_i8(%x: i8) -> i8 {
    %d = arith.addi %x, %x : i8
    %y = arith.addi %d, %x : i8
    return %y : i8
  }

  func.func @int_k4_i8(%x: i8) -> i8 {
    %sq = arith.muli %x, %x : i8
    %y = arith.addi %sq, %x : i8
    return %y : i8
  }

  func.func @int_k5_i8(%x: i8) -> i8 {
    %d = arith.addi %x, %x : i8
    %y = arith.muli %d, %d : i8
    return %y : i8
  }

  func.func @int_k6_i8(%x: i8) -> i8 {
    %sq = arith.muli %x, %x : i8
    %y = arith.muli %sq, %x : i8
    return %y : i8
  }

  func.func @int_k7_i8(%x: i8) -> i8 {
    %sq = arith.muli %x, %x : i8
    %y = arith.addi %sq, %sq : i8
    return %y : i8
  }

  func.func @int_k8_i8(%x: i8) -> i8 {
    %d = arith.addi %x, %x : i8
    %t0 = arith.addi %d, %x : i8
    %y = arith.muli %t0, %x : i8
    return %y : i8
  }

  func.func @int_k1_i16(%x: i16) -> i16 {
    %y = arith.addi %x, %x : i16
    return %y : i16
  }

  func.func @int_k2_i16(%x: i16) -> i16 {
    %y = arith.muli %x, %x : i16
    return %y : i16
  }

  func.func @int_k3_i16(%x: i16) -> i16 {
    %d = arith.addi %x, %x : i16
    %y = arith.addi %d, %x : i16
    return %y : i16
  }

  func.func @int_k4_i16(%x: i16) -> i16 {
    %sq = arith.muli %x, %x : i16
    %y = arith.addi %sq, %x : i16
    return %y : i16
  }

  func.func @int_k5_i16(%x: i16) -> i16 {
    %d = arith.addi %x, %x : i16
    %y = arith.muli %d, %d : i16
    return %y : i16
  }

  func.func @int_k6_i16(%x: i16) -> i16 {
    %sq = arith.muli %x, %x : i16
    %y = arith.muli %sq, %x : i16
    return %y : i16
  }

  func.func @int_k7_i16(%x: i16) -> i16 {
    %sq = arith.muli %x, %x : i16
    %y = arith.addi %sq, %sq : i16
    return %y : i16
  }

  func.func @int_k8_i16(%x: i16) -> i16 {
    %d = arith.addi %x, %x : i16
    %t0 = arith.addi %d, %x : i16
    %y = arith.muli %t0, %x : i16
    return %y : i16
  }

  func.func @int_k1_i32(%x: i32) -> i32 {
    %y = arith.addi %x, %x : i32
    return %y : i32
  }

  func.func @int_k2_i32(%x: i32) -> i32 {
    %y = arith.muli %x, %x : i32
    return %y : i32
  }

  func.func @int_k3_i32(%x: i32) -> i32 {
    %d = arith.addi %x, %x : i32
    %y = arith.addi %d, %x : i32
    return %y : i32
  }

  func.func @int_k4_i32(%x: i32) -> i32 {
    %sq = arith.muli %x, %x : i32
    %y = arith.addi %sq, %x : i32
    return %y : i32
  }

  func.func @int_k5_i32(%x: i32) -> i32 {
    %d = arith.addi %x, %x : i32
    %y = arith.muli %d, %d : i32
    return %y : i32
  }

  func.func @int_k6_i32(%x: i32) -> i32 {
    %sq = arith.muli %x, %x : i32
    %y = arith.muli %sq, %x : i32
    return %y : i32
  }

  func.func @int_k7_i32(%x: i32) -> i32 {
    %sq = arith.muli %x, %x : i32
    %y = arith.addi %sq, %sq : i32
    return %y : i32
  }

  func.func @int_k8_i32(%x: i32) -> i32 {
    %d = arith.addi %x, %x : i32
    %t0 = arith.addi %d, %x : i32
    %y = arith.muli %t0, %x : i32
    return %y : i32
  }

  func.func @int_k1_i64(%x: i64) -> i64 {
    %y = arith.addi %x, %x : i64
    return %y : i64
  }

  func.func @int_k2_i64(%x: i64) -> i64 {
    %y = arith.muli %x, %x : i64
    return %y : i64
  }

  func.func @int_k3_i64(%x: i64) -> i64 {
    %d = arith.addi %x, %x : i64
    %y = arith.addi %d, %x : i64
    return %y : i64
  }

  func.func @int_k4_i64(%x: i64) -> i64 {
    %sq = arith.muli %x, %x : i64
    %y = arith.addi %sq, %x : i64
    return %y : i64
  }

  func.func @int_k5_i64(%x: i64) -> i64 {
    %d = arith.addi %x, %x : i64
    %y = arith.muli %d, %d : i64
    return %y : i64
  }

  func.func @int_k6_i64(%x: i64) -> i64 {
    %sq = arith.muli %x, %x : i64
    %y = arith.muli %sq, %x : i64
    return %y : i64
  }

  func.func @int_k7_i64(%x: i64) -> i64 {
    %sq = arith.muli %x, %x : i64
    %y = arith.addi %sq, %sq : i64
    return %y : i64
  }

  func.func @int_k8_i64(%x: i64) -> i64 {
    %d = arith.addi %x, %x : i64
    %t0 = arith.addi %d, %x : i64
    %y = arith.muli %t0, %x : i64
    return %y : i64
  }

  func.func @float_k1_f32(%x: f32) -> f32 {
    %y = arith.addf %x, %x : f32
    return %y : f32
  }

  func.func @float_k2_f32(%x: f32) -> f32 {
    %y = arith.mulf %x, %x : f32
    return %y : f32
  }

  func.func @float_k3_f32(%x: f32) -> f32 {
    %d = arith.addf %x, %x : f32
    %y = arith.addf %d, %x : f32
    return %y : f32
  }

  func.func @float_k4_f32(%x: f32) -> f32 {
    %sq = arith.mulf %x, %x : f32
    %y = arith.addf %sq, %x : f32
    return %y : f32
  }

  func.func @float_k5_f32(%x: f32) -> f32 {
    %d = arith.addf %x, %x : f32
    %y = arith.mulf %d, %d : f32
    return %y : f32
  }

  func.func @float_k6_f32(%x: f32) -> f32 {
    %sq = arith.mulf %x, %x : f32
    %y = arith.mulf %sq, %x : f32
    return %y : f32
  }

  func.func @float_k7_f32(%x: f32) -> f32 {
    %sq = arith.mulf %x, %x : f32
    %y = arith.addf %sq, %sq : f32
    return %y : f32
  }

  func.func @float_k8_f32(%x: f32) -> f32 {
    %d = arith.addf %x, %x : f32
    %t0 = arith.addf %d, %x : f32
    %y = arith.mulf %t0, %x : f32
    return %y : f32
  }

  func.func @float_k1_f64(%x: f64) -> f64 {
    %y = arith.addf %x, %x : f64
    return %y : f64
  }

  func.func @float_k2_f64(%x: f64) -> f64 {
    %y = arith.mulf %x, %x : f64
    return %y : f64
  }

  func.func @float_k3_f64(%x: f64) -> f64 {
    %d = arith.addf %x, %x : f64
    %y = arith.addf %d, %x : f64
    return %y : f64
  }

  func.func @float_k4_f64(%x: f64) -> f64 {
    %sq = arith.mulf %x, %x : f64
    %y = arith.addf %sq, %x : f64
    return %y : f64
  }

  func.func @float_k5_f64(%x: f64) -> f64 {
    %d = arith.addf %x, %x : f64
    %y = arith.mulf %d, %d : f64
    return %y : f64
  }

  func.func @float_k6_f64(%x: f64) -> f64 {
    %sq = arith.mulf %x, %x : f64
    %y = arith.mulf %sq, %x : f64
    return %y : f64
  }

  func.func @float_k7_f64(%x: f64) -> f64 {
    %sq = arith.mulf %x, %x : f64
    %y = arith.addf %sq, %sq : f64
    return %y : f64
  }

  func.func @float_k8_f64(%x: f64) -> f64 {
    %d = arith.addf %x, %x : f64
    %t0 = arith.addf %d, %x : f64
    %y = arith.mulf %t0, %x : f64
    return %y : f64
  }

  func.func @shared_polymorphic_kernel_bank_multitype(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
    %int_k1_i8 = call @int_k1_i8(%i8v) : (i8) -> i8
    %int_k2_i8 = call @int_k2_i8(%i8v) : (i8) -> i8
    %int_k3_i8 = call @int_k3_i8(%i8v) : (i8) -> i8
    %int_k4_i8 = call @int_k4_i8(%i8v) : (i8) -> i8
    %int_k5_i8 = call @int_k5_i8(%i8v) : (i8) -> i8
    %int_k6_i8 = call @int_k6_i8(%i8v) : (i8) -> i8
    %int_k7_i8 = call @int_k7_i8(%i8v) : (i8) -> i8
    %int_k8_i8 = call @int_k8_i8(%i8v) : (i8) -> i8
    %int_k1_i16 = call @int_k1_i16(%i16v) : (i16) -> i16
    %int_k2_i16 = call @int_k2_i16(%i16v) : (i16) -> i16
    %int_k3_i16 = call @int_k3_i16(%i16v) : (i16) -> i16
    %int_k4_i16 = call @int_k4_i16(%i16v) : (i16) -> i16
    %int_k5_i16 = call @int_k5_i16(%i16v) : (i16) -> i16
    %int_k6_i16 = call @int_k6_i16(%i16v) : (i16) -> i16
    %int_k7_i16 = call @int_k7_i16(%i16v) : (i16) -> i16
    %int_k8_i16 = call @int_k8_i16(%i16v) : (i16) -> i16
    %int_k1_i32 = call @int_k1_i32(%i32v) : (i32) -> i32
    %int_k2_i32 = call @int_k2_i32(%i32v) : (i32) -> i32
    %int_k3_i32 = call @int_k3_i32(%i32v) : (i32) -> i32
    %int_k4_i32 = call @int_k4_i32(%i32v) : (i32) -> i32
    %int_k5_i32 = call @int_k5_i32(%i32v) : (i32) -> i32
    %int_k6_i32 = call @int_k6_i32(%i32v) : (i32) -> i32
    %int_k7_i32 = call @int_k7_i32(%i32v) : (i32) -> i32
    %int_k8_i32 = call @int_k8_i32(%i32v) : (i32) -> i32
    %int_k1_i64 = call @int_k1_i64(%i64v) : (i64) -> i64
    %int_k2_i64 = call @int_k2_i64(%i64v) : (i64) -> i64
    %int_k3_i64 = call @int_k3_i64(%i64v) : (i64) -> i64
    %int_k4_i64 = call @int_k4_i64(%i64v) : (i64) -> i64
    %int_k5_i64 = call @int_k5_i64(%i64v) : (i64) -> i64
    %int_k6_i64 = call @int_k6_i64(%i64v) : (i64) -> i64
    %int_k7_i64 = call @int_k7_i64(%i64v) : (i64) -> i64
    %int_k8_i64 = call @int_k8_i64(%i64v) : (i64) -> i64
    %float_k1_f32 = call @float_k1_f32(%f32v) : (f32) -> f32
    %float_k2_f32 = call @float_k2_f32(%f32v) : (f32) -> f32
    %float_k3_f32 = call @float_k3_f32(%f32v) : (f32) -> f32
    %float_k4_f32 = call @float_k4_f32(%f32v) : (f32) -> f32
    %float_k5_f32 = call @float_k5_f32(%f32v) : (f32) -> f32
    %float_k6_f32 = call @float_k6_f32(%f32v) : (f32) -> f32
    %float_k7_f32 = call @float_k7_f32(%f32v) : (f32) -> f32
    %float_k8_f32 = call @float_k8_f32(%f32v) : (f32) -> f32
    %float_k1_f64 = call @float_k1_f64(%f64v) : (f64) -> f64
    %float_k2_f64 = call @float_k2_f64(%f64v) : (f64) -> f64
    %float_k3_f64 = call @float_k3_f64(%f64v) : (f64) -> f64
    %float_k4_f64 = call @float_k4_f64(%f64v) : (f64) -> f64
    %float_k5_f64 = call @float_k5_f64(%f64v) : (f64) -> f64
    %float_k6_f64 = call @float_k6_f64(%f64v) : (f64) -> f64
    %float_k7_f64 = call @float_k7_f64(%f64v) : (f64) -> f64
    %float_k8_f64 = call @float_k8_f64(%f64v) : (f64) -> f64

    %ext_k1_i8 = arith.extsi %int_k1_i8 : i8 to i64
    %ext_k2_i8 = arith.extsi %int_k2_i8 : i8 to i64
    %ext_k3_i8 = arith.extsi %int_k3_i8 : i8 to i64
    %ext_k4_i8 = arith.extsi %int_k4_i8 : i8 to i64
    %ext_k5_i8 = arith.extsi %int_k5_i8 : i8 to i64
    %ext_k6_i8 = arith.extsi %int_k6_i8 : i8 to i64
    %ext_k7_i8 = arith.extsi %int_k7_i8 : i8 to i64
    %ext_k8_i8 = arith.extsi %int_k8_i8 : i8 to i64
    %ext_k1_i16 = arith.extsi %int_k1_i16 : i16 to i64
    %ext_k2_i16 = arith.extsi %int_k2_i16 : i16 to i64
    %ext_k3_i16 = arith.extsi %int_k3_i16 : i16 to i64
    %ext_k4_i16 = arith.extsi %int_k4_i16 : i16 to i64
    %ext_k5_i16 = arith.extsi %int_k5_i16 : i16 to i64
    %ext_k6_i16 = arith.extsi %int_k6_i16 : i16 to i64
    %ext_k7_i16 = arith.extsi %int_k7_i16 : i16 to i64
    %ext_k8_i16 = arith.extsi %int_k8_i16 : i16 to i64
    %ext_k1_i32 = arith.extsi %int_k1_i32 : i32 to i64
    %ext_k2_i32 = arith.extsi %int_k2_i32 : i32 to i64
    %ext_k3_i32 = arith.extsi %int_k3_i32 : i32 to i64
    %ext_k4_i32 = arith.extsi %int_k4_i32 : i32 to i64
    %ext_k5_i32 = arith.extsi %int_k5_i32 : i32 to i64
    %ext_k6_i32 = arith.extsi %int_k6_i32 : i32 to i64
    %ext_k7_i32 = arith.extsi %int_k7_i32 : i32 to i64
    %ext_k8_i32 = arith.extsi %int_k8_i32 : i32 to i64
    %cast_k1_f32 = arith.fptosi %float_k1_f32 : f32 to i64
    %cast_k2_f32 = arith.fptosi %float_k2_f32 : f32 to i64
    %cast_k3_f32 = arith.fptosi %float_k3_f32 : f32 to i64
    %cast_k4_f32 = arith.fptosi %float_k4_f32 : f32 to i64
    %cast_k5_f32 = arith.fptosi %float_k5_f32 : f32 to i64
    %cast_k6_f32 = arith.fptosi %float_k6_f32 : f32 to i64
    %cast_k7_f32 = arith.fptosi %float_k7_f32 : f32 to i64
    %cast_k8_f32 = arith.fptosi %float_k8_f32 : f32 to i64
    %cast_k1_f64 = arith.fptosi %float_k1_f64 : f64 to i64
    %cast_k2_f64 = arith.fptosi %float_k2_f64 : f64 to i64
    %cast_k3_f64 = arith.fptosi %float_k3_f64 : f64 to i64
    %cast_k4_f64 = arith.fptosi %float_k4_f64 : f64 to i64
    %cast_k5_f64 = arith.fptosi %float_k5_f64 : f64 to i64
    %cast_k6_f64 = arith.fptosi %float_k6_f64 : f64 to i64
    %cast_k7_f64 = arith.fptosi %float_k7_f64 : f64 to i64
    %cast_k8_f64 = arith.fptosi %float_k8_f64 : f64 to i64

    %sum_1 = arith.addi %ext_k1_i8, %ext_k2_i8 : i64
    %sum_2 = arith.addi %sum_1, %ext_k3_i8 : i64
    %sum_3 = arith.addi %sum_2, %ext_k4_i8 : i64
    %sum_4 = arith.addi %sum_3, %ext_k5_i8 : i64
    %sum_5 = arith.addi %sum_4, %ext_k6_i8 : i64
    %sum_6 = arith.addi %sum_5, %ext_k7_i8 : i64
    %sum_7 = arith.addi %sum_6, %ext_k8_i8 : i64
    %sum_8 = arith.addi %sum_7, %ext_k1_i16 : i64
    %sum_9 = arith.addi %sum_8, %ext_k2_i16 : i64
    %sum_10 = arith.addi %sum_9, %ext_k3_i16 : i64
    %sum_11 = arith.addi %sum_10, %ext_k4_i16 : i64
    %sum_12 = arith.addi %sum_11, %ext_k5_i16 : i64
    %sum_13 = arith.addi %sum_12, %ext_k6_i16 : i64
    %sum_14 = arith.addi %sum_13, %ext_k7_i16 : i64
    %sum_15 = arith.addi %sum_14, %ext_k8_i16 : i64
    %sum_16 = arith.addi %sum_15, %ext_k1_i32 : i64
    %sum_17 = arith.addi %sum_16, %ext_k2_i32 : i64
    %sum_18 = arith.addi %sum_17, %ext_k3_i32 : i64
    %sum_19 = arith.addi %sum_18, %ext_k4_i32 : i64
    %sum_20 = arith.addi %sum_19, %ext_k5_i32 : i64
    %sum_21 = arith.addi %sum_20, %ext_k6_i32 : i64
    %sum_22 = arith.addi %sum_21, %ext_k7_i32 : i64
    %sum_23 = arith.addi %sum_22, %ext_k8_i32 : i64
    %sum_24 = arith.addi %sum_23, %int_k1_i64 : i64
    %sum_25 = arith.addi %sum_24, %int_k2_i64 : i64
    %sum_26 = arith.addi %sum_25, %int_k3_i64 : i64
    %sum_27 = arith.addi %sum_26, %int_k4_i64 : i64
    %sum_28 = arith.addi %sum_27, %int_k5_i64 : i64
    %sum_29 = arith.addi %sum_28, %int_k6_i64 : i64
    %sum_30 = arith.addi %sum_29, %int_k7_i64 : i64
    %sum_31 = arith.addi %sum_30, %int_k8_i64 : i64
    %sum_32 = arith.addi %sum_31, %cast_k1_f32 : i64
    %sum_33 = arith.addi %sum_32, %cast_k2_f32 : i64
    %sum_34 = arith.addi %sum_33, %cast_k3_f32 : i64
    %sum_35 = arith.addi %sum_34, %cast_k4_f32 : i64
    %sum_36 = arith.addi %sum_35, %cast_k5_f32 : i64
    %sum_37 = arith.addi %sum_36, %cast_k6_f32 : i64
    %sum_38 = arith.addi %sum_37, %cast_k7_f32 : i64
    %sum_39 = arith.addi %sum_38, %cast_k8_f32 : i64
    %sum_40 = arith.addi %sum_39, %cast_k1_f64 : i64
    %sum_41 = arith.addi %sum_40, %cast_k2_f64 : i64
    %sum_42 = arith.addi %sum_41, %cast_k3_f64 : i64
    %sum_43 = arith.addi %sum_42, %cast_k4_f64 : i64
    %sum_44 = arith.addi %sum_43, %cast_k5_f64 : i64
    %sum_45 = arith.addi %sum_44, %cast_k6_f64 : i64
    %sum_46 = arith.addi %sum_45, %cast_k7_f64 : i64
    %sum_47 = arith.addi %sum_46, %cast_k8_f64 : i64
    return %sum_47 : i64
  }
}

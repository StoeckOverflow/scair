module {
  func.func @sink_i8(%x: i8) -> i8 { return %x : i8 }
  func.func @sink_i16(%x: i16) -> i16 { return %x : i16 }
  func.func @sink_i32(%x: i32) -> i32 { return %x : i32 }
  func.func @sink_i64(%x: i64) -> i64 { return %x : i64 }
  func.func @sink_f32(%x: f32) -> f32 { return %x : f32 }
  func.func @sink_f64(%x: f64) -> f64 { return %x : f64 }

  func.func @map_i8(%x: i8) -> i8 {
    %v = call @sink_i8(%x) : (i8) -> i8
    %c = arith.constant 1 : i8
    %y = arith.addi %v, %c : i8
    return %y : i8
  }
  func.func @map_i16(%x: i16) -> i16 {
    %v = call @sink_i16(%x) : (i16) -> i16
    %c = arith.constant 2 : i16
    %y = arith.muli %v, %c : i16
    return %y : i16
  }
  func.func @map_i32(%x: i32) -> i32 {
    %v = call @sink_i32(%x) : (i32) -> i32
    %c = arith.constant 3 : i32
    %y = arith.subi %v, %c : i32
    return %y : i32
  }
  func.func @map_i64(%x: i64) -> i64 {
    %v = call @sink_i64(%x) : (i64) -> i64
    %c = arith.constant 4 : i64
    %y = arith.addi %v, %c : i64
    return %y : i64
  }
  func.func @map_f32(%x: f32) -> f32 {
    %v = call @sink_f32(%x) : (f32) -> f32
    %c = arith.constant 1.250000e+00 : f32
    %y = arith.mulf %v, %c : f32
    return %y : f32
  }
  func.func @map_f64(%x: f64) -> f64 {
    %v = call @sink_f64(%x) : (f64) -> f64
    %c = arith.constant 7.500000e-01 : f64
    %y = arith.addf %v, %c : f64
    return %y : f64
  }

  func.func @repeated_polymorphic_map_chain(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
    %i8_1 = call @map_i8(%i8v) : (i8) -> i8
    %i8_2 = call @map_i8(%i8_1) : (i8) -> i8
    %r_i8 = call @map_i8(%i8_2) : (i8) -> i8

    %i16_1 = call @map_i16(%i16v) : (i16) -> i16
    %i16_2 = call @map_i16(%i16_1) : (i16) -> i16
    %r_i16 = call @map_i16(%i16_2) : (i16) -> i16

    %i32_1 = call @map_i32(%i32v) : (i32) -> i32
    %i32_2 = call @map_i32(%i32_1) : (i32) -> i32
    %r_i32 = call @map_i32(%i32_2) : (i32) -> i32

    %i64_1 = call @map_i64(%i64v) : (i64) -> i64
    %i64_2 = call @map_i64(%i64_1) : (i64) -> i64
    %r_i64 = call @map_i64(%i64_2) : (i64) -> i64

    %f32_1 = call @map_f32(%f32v) : (f32) -> f32
    %f32_2 = call @map_f32(%f32_1) : (f32) -> f32
    %r_f32 = call @map_f32(%f32_2) : (f32) -> f32

    %f64_1 = call @map_f64(%f64v) : (f64) -> f64
    %f64_2 = call @map_f64(%f64_1) : (f64) -> f64
    %r_f64 = call @map_f64(%f64_2) : (f64) -> f64

    %s_i8 = arith.extsi %r_i8 : i8 to i64
    %s_i16 = arith.extsi %r_i16 : i16 to i64
    %s_i32 = arith.extsi %r_i32 : i32 to i64
    %s_f32 = arith.fptosi %r_f32 : f32 to i64
    %s_f64 = arith.fptosi %r_f64 : f64 to i64
    %a0 = arith.addi %s_i8, %s_i16 : i64
    %a1 = arith.addi %s_i32, %r_i64 : i64
    %a2 = arith.addi %s_f32, %s_f64 : i64
    %a3 = arith.addi %a0, %a1 : i64
    %sum = arith.addi %a3, %a2 : i64
    return %sum : i64
  }
}

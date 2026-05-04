module {
  func.func @sink_i8(%x: i8) -> i8 { return %x : i8 }
  func.func @sink_i16(%x: i16) -> i16 { return %x : i16 }
  func.func @sink_i32(%x: i32) -> i32 { return %x : i32 }
  func.func @sink_i64(%x: i64) -> i64 { return %x : i64 }
  func.func @sink_f32(%x: f32) -> f32 { return %x : f32 }
  func.func @sink_f64(%x: f64) -> f64 { return %x : f64 }

  func.func @shared_polymorphic_identity_multitype(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
    %r_i8 = call @sink_i8(%i8v) : (i8) -> i8
    %r_i16 = call @sink_i16(%i16v) : (i16) -> i16
    %r_i32 = call @sink_i32(%i32v) : (i32) -> i32
    %r_i64 = call @sink_i64(%i64v) : (i64) -> i64
    %r_f32 = call @sink_f32(%f32v) : (f32) -> f32
    %r_f64 = call @sink_f64(%f64v) : (f64) -> f64
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

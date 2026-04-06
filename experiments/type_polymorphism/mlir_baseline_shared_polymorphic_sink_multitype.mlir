// Benchmark purpose: monomorphic MLIR baseline for compose-like fanout.
// Polymorphic combinator shape being simulated: conceptually forall T. (T -> T) -> (T -> T) -> T -> T;
// executable realization here duplicates a monomorphic sink and a typed compose wrapper per concrete type.
// Scaling knobs: fixed type fanout over i8, i16, i32, i64, f32, f64; two worker steps per type.
// Expected comparison story: this file duplicates sink and compose structure per concrete type.
module {
  func.func @sink_i8(%x: i8) -> i8 { return %x : i8 }
  func.func @sink_i16(%x: i16) -> i16 { return %x : i16 }
  func.func @sink_i32(%x: i32) -> i32 { return %x : i32 }
  func.func @sink_i64(%x: i64) -> i64 { return %x : i64 }
  func.func @sink_f32(%x: f32) -> f32 { return %x : f32 }
  func.func @sink_f64(%x: f64) -> f64 { return %x : f64 }

  func.func @compose_i8(%x: i8) -> i8 {
    %s = call @sink_i8(%x) : (i8) -> i8
    %c0 = arith.constant 3 : i8
    %f = arith.addi %s, %c0 : i8
    %c1 = arith.constant 2 : i8
    %r = arith.muli %f, %c1 : i8
    return %r : i8
  }

  func.func @compose_i16(%x: i16) -> i16 {
    %s = call @sink_i16(%x) : (i16) -> i16
    %c0 = arith.constant 5 : i16
    %f = arith.addi %s, %c0 : i16
    %c1 = arith.constant 11 : i16
    %r = arith.addi %f, %c1 : i16
    return %r : i16
  }

  func.func @compose_i32(%x: i32) -> i32 {
    %s = call @sink_i32(%x) : (i32) -> i32
    %c0 = arith.constant 3 : i32
    %f = arith.muli %s, %c0 : i32
    %c1 = arith.constant 7 : i32
    %r = arith.subi %f, %c1 : i32
    return %r : i32
  }

  func.func @compose_i64(%x: i64) -> i64 {
    %s = call @sink_i64(%x) : (i64) -> i64
    %c0 = arith.constant 13 : i64
    %f = arith.addi %s, %c0 : i64
    %c1 = arith.constant 2 : i64
    %r = arith.muli %f, %c1 : i64
    return %r : i64
  }

  func.func @compose_f32(%x: f32) -> f32 {
    %s = call @sink_f32(%x) : (f32) -> f32
    %c0 = arith.constant 1.500000e+00 : f32
    %f = arith.mulf %s, %c0 : f32
    %c1 = arith.constant 2.250000e+00 : f32
    %r = arith.addf %f, %c1 : f32
    return %r : f32
  }

  func.func @compose_f64(%x: f64) -> f64 {
    %s = call @sink_f64(%x) : (f64) -> f64
    %c0 = arith.constant 3.500000e+00 : f64
    %f = arith.addf %s, %c0 : f64
    %c1 = arith.constant 5.000000e-01 : f64
    %r = arith.mulf %f, %c1 : f64
    return %r : f64
  }

  func.func @shared_polymorphic_sink_multitype(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
    %r_i8 = call @compose_i8(%i8v) : (i8) -> i8
    %r_i16 = call @compose_i16(%i16v) : (i16) -> i16
    %r_i32 = call @compose_i32(%i32v) : (i32) -> i32
    %r_i64 = call @compose_i64(%i64v) : (i64) -> i64
    %r_f32 = call @compose_f32(%f32v) : (f32) -> f32
    %r_f64 = call @compose_f64(%f64v) : (f64) -> f64
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

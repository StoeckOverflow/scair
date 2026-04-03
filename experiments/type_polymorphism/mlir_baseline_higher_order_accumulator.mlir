// Benchmark purpose: monomorphic MLIR baseline for accumulator-style design benchmark.
// Polymorphic combinator shape being simulated: conceptually forall T. (T -> i64) -> T -> i64;
// executable realization here duplicates a monomorphic sink and scorer path per concrete type.
// Scaling knobs: fixed type fanout over i8, i16, i32, i64, f32, f64; one scorer use per type.
// Expected comparison story: this file duplicates sink and scorer wrappers per type.
module {
  func.func @sink_i8(%x: i8) -> i8 { return %x : i8 }
  func.func @sink_i16(%x: i16) -> i16 { return %x : i16 }
  func.func @sink_i32(%x: i32) -> i32 { return %x : i32 }
  func.func @sink_i64(%x: i64) -> i64 { return %x : i64 }
  func.func @sink_f32(%x: f32) -> f32 { return %x : f32 }
  func.func @sink_f64(%x: f64) -> f64 { return %x : f64 }

  func.func @accum_i8(%x: i8) -> i64 {
    %v = call @sink_i8(%x) : (i8) -> i8
    %sx = arith.extsi %v : i8 to i64
    %c = arith.constant 7 : i64
    %y = arith.addi %sx, %c : i64
    return %y : i64
  }
  func.func @accum_i16(%x: i16) -> i64 {
    %v = call @sink_i16(%x) : (i16) -> i16
    %sx = arith.extsi %v : i16 to i64
    %c = arith.constant 2 : i64
    %y = arith.muli %sx, %c : i64
    return %y : i64
  }
  func.func @accum_i32(%x: i32) -> i64 {
    %v = call @sink_i32(%x) : (i32) -> i32
    %sx = arith.extsi %v : i32 to i64
    %c = arith.constant 9 : i64
    %y = arith.subi %sx, %c : i64
    return %y : i64
  }
  func.func @accum_i64(%x: i64) -> i64 {
    %v = call @sink_i64(%x) : (i64) -> i64
    %c = arith.constant 5 : i64
    %y = arith.addi %v, %c : i64
    return %y : i64
  }
  func.func @accum_f32(%x: f32) -> i64 {
    %v = call @sink_f32(%x) : (f32) -> f32
    %m = arith.constant 2.000000e+00 : f32
    %a = arith.constant 1.000000e+00 : f32
    %mx = arith.mulf %v, %m : f32
    %ax = arith.addf %mx, %a : f32
    %y = arith.fptosi %ax : f32 to i64
    return %y : i64
  }
  func.func @accum_f64(%x: f64) -> i64 {
    %v = call @sink_f64(%x) : (f64) -> f64
    %m = arith.constant 5.000000e-01 : f64
    %a = arith.constant 1.100000e+01 : f64
    %mx = arith.mulf %v, %m : f64
    %ax = arith.addf %mx, %a : f64
    %y = arith.fptosi %ax : f64 to i64
    return %y : i64
  }

  func.func @higher_order_accumulator(%i8v: i8, %i16v: i16, %i32v: i32, %i64v: i64, %f32v: f32, %f64v: f64) -> i64 {
    %r_i8 = call @accum_i8(%i8v) : (i8) -> i64
    %r_i16 = call @accum_i16(%i16v) : (i16) -> i64
    %r_i32 = call @accum_i32(%i32v) : (i32) -> i64
    %r_i64 = call @accum_i64(%i64v) : (i64) -> i64
    %r_f32 = call @accum_f32(%f32v) : (f32) -> i64
    %r_f64 = call @accum_f64(%f64v) : (f64) -> i64
    %a0 = arith.addi %r_i8, %r_i16 : i64
    %a1 = arith.addi %r_i32, %r_i64 : i64
    %a2 = arith.addi %r_f32, %r_f64 : i64
    %a3 = arith.addi %a0, %a1 : i64
    %sum = arith.addi %a3, %a2 : i64
    return %sum : i64
  }
}

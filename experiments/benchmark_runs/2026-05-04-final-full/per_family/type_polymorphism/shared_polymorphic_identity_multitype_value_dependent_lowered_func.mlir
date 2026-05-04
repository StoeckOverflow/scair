builtin.module {
  func.func @lifted_6(%0: f64) -> f64 {
    func.return %0 : f64
  }
  func.func @lifted_5(%0: f32) -> f32 {
    func.return %0 : f32
  }
  func.func @lifted_4(%0: i64) -> i64 {
    func.return %0 : i64
  }
  func.func @lifted_3(%0: i32) -> i32 {
    func.return %0 : i32
  }
  func.func @lifted_2(%0: i16) -> i16 {
    func.return %0 : i16
  }
  func.func @lifted_1(%0: i8) -> i8 {
    func.return %0 : i8
  }
  func.func @shared_polymorphic_identity_multitype(%0: i8, %1: i16, %2: i32, %3: i64, %4: f32, %5: f64) -> i64 {
    %6 = "arith.extsi"(%0) : (i8) -> i64
    %7 = "arith.extsi"(%1) : (i16) -> i64
    %8 = "arith.extsi"(%2) : (i32) -> i64
    %9 = "arith.fptosi"(%4) : (f32) -> i64
    %10 = "arith.fptosi"(%5) : (f64) -> i64
    %11 = "arith.addi"(%6, %7) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %12 = "arith.addi"(%8, %3) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %13 = "arith.addi"(%9, %10) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %14 = "arith.addi"(%11, %12) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    %15 = "arith.addi"(%14, %13) <{overflowFlags = #arith.overflow<none>}> : (i64, i64) -> i64
    func.return %15 : i64
  }
}

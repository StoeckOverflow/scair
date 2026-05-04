module {
  llvm.func @sink_i8(%arg0: i8) -> i8 {
    llvm.return %arg0 : i8
  }
  llvm.func @sink_i16(%arg0: i16) -> i16 {
    llvm.return %arg0 : i16
  }
  llvm.func @sink_i32(%arg0: i32) -> i32 {
    llvm.return %arg0 : i32
  }
  llvm.func @sink_i64(%arg0: i64) -> i64 {
    llvm.return %arg0 : i64
  }
  llvm.func @sink_f32(%arg0: f32) -> f32 {
    llvm.return %arg0 : f32
  }
  llvm.func @sink_f64(%arg0: f64) -> f64 {
    llvm.return %arg0 : f64
  }
  llvm.func @shared_polymorphic_identity_multitype(%arg0: i8, %arg1: i16, %arg2: i32, %arg3: i64, %arg4: f32, %arg5: f64) -> i64 {
    %0 = llvm.call @sink_i8(%arg0) : (i8) -> i8
    %1 = llvm.call @sink_i16(%arg1) : (i16) -> i16
    %2 = llvm.call @sink_i32(%arg2) : (i32) -> i32
    %3 = llvm.call @sink_i64(%arg3) : (i64) -> i64
    %4 = llvm.call @sink_f32(%arg4) : (f32) -> f32
    %5 = llvm.call @sink_f64(%arg5) : (f64) -> f64
    %6 = llvm.sext %0 : i8 to i64
    %7 = llvm.sext %1 : i16 to i64
    %8 = llvm.sext %2 : i32 to i64
    %9 = llvm.fptosi %4 : f32 to i64
    %10 = llvm.fptosi %5 : f64 to i64
    %11 = llvm.add %6, %7 : i64
    %12 = llvm.add %8, %3 : i64
    %13 = llvm.add %9, %10 : i64
    %14 = llvm.add %11, %12 : i64
    %15 = llvm.add %14, %13 : i64
    llvm.return %15 : i64
  }
}


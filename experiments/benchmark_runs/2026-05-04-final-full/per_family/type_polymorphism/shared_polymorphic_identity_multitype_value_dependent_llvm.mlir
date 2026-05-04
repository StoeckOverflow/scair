module {
  llvm.func @lifted_6(%arg0: f64) -> f64 {
    llvm.return %arg0 : f64
  }
  llvm.func @lifted_5(%arg0: f32) -> f32 {
    llvm.return %arg0 : f32
  }
  llvm.func @lifted_4(%arg0: i64) -> i64 {
    llvm.return %arg0 : i64
  }
  llvm.func @lifted_3(%arg0: i32) -> i32 {
    llvm.return %arg0 : i32
  }
  llvm.func @lifted_2(%arg0: i16) -> i16 {
    llvm.return %arg0 : i16
  }
  llvm.func @lifted_1(%arg0: i8) -> i8 {
    llvm.return %arg0 : i8
  }
  llvm.func @shared_polymorphic_identity_multitype(%arg0: i8, %arg1: i16, %arg2: i32, %arg3: i64, %arg4: f32, %arg5: f64) -> i64 {
    %0 = llvm.sext %arg0 : i8 to i64
    %1 = llvm.sext %arg1 : i16 to i64
    %2 = llvm.sext %arg2 : i32 to i64
    %3 = llvm.fptosi %arg4 : f32 to i64
    %4 = llvm.fptosi %arg5 : f64 to i64
    %5 = llvm.add %0, %1 : i64
    %6 = llvm.add %2, %arg3 : i64
    %7 = llvm.add %3, %4 : i64
    %8 = llvm.add %5, %6 : i64
    %9 = llvm.add %8, %7 : i64
    llvm.return %9 : i64
  }
}


module {
  llvm.func @lifted_48(%arg0: f64) -> f64 {
    %0 = llvm.fadd %arg0, %arg0 : f64
    %1 = llvm.fadd %0, %arg0 : f64
    %2 = llvm.fmul %1, %arg0 : f64
    llvm.return %2 : f64
  }
  llvm.func @lifted_47(%arg0: f64) -> f64 {
    %0 = llvm.fmul %arg0, %arg0 : f64
    %1 = llvm.fadd %0, %0 : f64
    llvm.return %1 : f64
  }
  llvm.func @lifted_46(%arg0: f64) -> f64 {
    %0 = llvm.fmul %arg0, %arg0 : f64
    %1 = llvm.fmul %0, %arg0 : f64
    llvm.return %1 : f64
  }
  llvm.func @lifted_45(%arg0: f64) -> f64 {
    %0 = llvm.fadd %arg0, %arg0 : f64
    %1 = llvm.fmul %0, %0 : f64
    llvm.return %1 : f64
  }
  llvm.func @lifted_44(%arg0: f64) -> f64 {
    %0 = llvm.fmul %arg0, %arg0 : f64
    %1 = llvm.fadd %0, %arg0 : f64
    llvm.return %1 : f64
  }
  llvm.func @lifted_43(%arg0: f64) -> f64 {
    %0 = llvm.fadd %arg0, %arg0 : f64
    %1 = llvm.fadd %0, %arg0 : f64
    llvm.return %1 : f64
  }
  llvm.func @lifted_42(%arg0: f64) -> f64 {
    %0 = llvm.fmul %arg0, %arg0 : f64
    llvm.return %0 : f64
  }
  llvm.func @lifted_41(%arg0: f64) -> f64 {
    %0 = llvm.fadd %arg0, %arg0 : f64
    llvm.return %0 : f64
  }
  llvm.func @lifted_40(%arg0: f32) -> f32 {
    %0 = llvm.fadd %arg0, %arg0 : f32
    %1 = llvm.fadd %0, %arg0 : f32
    %2 = llvm.fmul %1, %arg0 : f32
    llvm.return %2 : f32
  }
  llvm.func @lifted_39(%arg0: f32) -> f32 {
    %0 = llvm.fmul %arg0, %arg0 : f32
    %1 = llvm.fadd %0, %0 : f32
    llvm.return %1 : f32
  }
  llvm.func @lifted_38(%arg0: f32) -> f32 {
    %0 = llvm.fmul %arg0, %arg0 : f32
    %1 = llvm.fmul %0, %arg0 : f32
    llvm.return %1 : f32
  }
  llvm.func @lifted_37(%arg0: f32) -> f32 {
    %0 = llvm.fadd %arg0, %arg0 : f32
    %1 = llvm.fmul %0, %0 : f32
    llvm.return %1 : f32
  }
  llvm.func @lifted_36(%arg0: f32) -> f32 {
    %0 = llvm.fmul %arg0, %arg0 : f32
    %1 = llvm.fadd %0, %arg0 : f32
    llvm.return %1 : f32
  }
  llvm.func @lifted_35(%arg0: f32) -> f32 {
    %0 = llvm.fadd %arg0, %arg0 : f32
    %1 = llvm.fadd %0, %arg0 : f32
    llvm.return %1 : f32
  }
  llvm.func @lifted_34(%arg0: f32) -> f32 {
    %0 = llvm.fmul %arg0, %arg0 : f32
    llvm.return %0 : f32
  }
  llvm.func @lifted_33(%arg0: f32) -> f32 {
    %0 = llvm.fadd %arg0, %arg0 : f32
    llvm.return %0 : f32
  }
  llvm.func @lifted_32(%arg0: i64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i64
    %1 = llvm.add %0, %arg0 : i64
    %2 = llvm.mul %1, %arg0 : i64
    llvm.return %2 : i64
  }
  llvm.func @lifted_31(%arg0: i64) -> i64 {
    %0 = llvm.mul %arg0, %arg0 : i64
    %1 = llvm.add %0, %0 : i64
    llvm.return %1 : i64
  }
  llvm.func @lifted_30(%arg0: i64) -> i64 {
    %0 = llvm.mul %arg0, %arg0 : i64
    %1 = llvm.mul %0, %arg0 : i64
    llvm.return %1 : i64
  }
  llvm.func @lifted_29(%arg0: i64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i64
    %1 = llvm.mul %0, %0 : i64
    llvm.return %1 : i64
  }
  llvm.func @lifted_28(%arg0: i64) -> i64 {
    %0 = llvm.mul %arg0, %arg0 : i64
    %1 = llvm.add %0, %arg0 : i64
    llvm.return %1 : i64
  }
  llvm.func @lifted_27(%arg0: i64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i64
    %1 = llvm.add %0, %arg0 : i64
    llvm.return %1 : i64
  }
  llvm.func @lifted_26(%arg0: i64) -> i64 {
    %0 = llvm.mul %arg0, %arg0 : i64
    llvm.return %0 : i64
  }
  llvm.func @lifted_25(%arg0: i64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i64
    llvm.return %0 : i64
  }
  llvm.func @lifted_24(%arg0: i32) -> i32 {
    %0 = llvm.add %arg0, %arg0 : i32
    %1 = llvm.add %0, %arg0 : i32
    %2 = llvm.mul %1, %arg0 : i32
    llvm.return %2 : i32
  }
  llvm.func @lifted_23(%arg0: i32) -> i32 {
    %0 = llvm.mul %arg0, %arg0 : i32
    %1 = llvm.add %0, %0 : i32
    llvm.return %1 : i32
  }
  llvm.func @lifted_22(%arg0: i32) -> i32 {
    %0 = llvm.mul %arg0, %arg0 : i32
    %1 = llvm.mul %0, %arg0 : i32
    llvm.return %1 : i32
  }
  llvm.func @lifted_21(%arg0: i32) -> i32 {
    %0 = llvm.add %arg0, %arg0 : i32
    %1 = llvm.mul %0, %0 : i32
    llvm.return %1 : i32
  }
  llvm.func @lifted_20(%arg0: i32) -> i32 {
    %0 = llvm.mul %arg0, %arg0 : i32
    %1 = llvm.add %0, %arg0 : i32
    llvm.return %1 : i32
  }
  llvm.func @lifted_19(%arg0: i32) -> i32 {
    %0 = llvm.add %arg0, %arg0 : i32
    %1 = llvm.add %0, %arg0 : i32
    llvm.return %1 : i32
  }
  llvm.func @lifted_18(%arg0: i32) -> i32 {
    %0 = llvm.mul %arg0, %arg0 : i32
    llvm.return %0 : i32
  }
  llvm.func @lifted_17(%arg0: i32) -> i32 {
    %0 = llvm.add %arg0, %arg0 : i32
    llvm.return %0 : i32
  }
  llvm.func @lifted_16(%arg0: i16) -> i16 {
    %0 = llvm.add %arg0, %arg0 : i16
    %1 = llvm.add %0, %arg0 : i16
    %2 = llvm.mul %1, %arg0 : i16
    llvm.return %2 : i16
  }
  llvm.func @lifted_15(%arg0: i16) -> i16 {
    %0 = llvm.mul %arg0, %arg0 : i16
    %1 = llvm.add %0, %0 : i16
    llvm.return %1 : i16
  }
  llvm.func @lifted_14(%arg0: i16) -> i16 {
    %0 = llvm.mul %arg0, %arg0 : i16
    %1 = llvm.mul %0, %arg0 : i16
    llvm.return %1 : i16
  }
  llvm.func @lifted_13(%arg0: i16) -> i16 {
    %0 = llvm.add %arg0, %arg0 : i16
    %1 = llvm.mul %0, %0 : i16
    llvm.return %1 : i16
  }
  llvm.func @lifted_12(%arg0: i16) -> i16 {
    %0 = llvm.mul %arg0, %arg0 : i16
    %1 = llvm.add %0, %arg0 : i16
    llvm.return %1 : i16
  }
  llvm.func @lifted_11(%arg0: i16) -> i16 {
    %0 = llvm.add %arg0, %arg0 : i16
    %1 = llvm.add %0, %arg0 : i16
    llvm.return %1 : i16
  }
  llvm.func @lifted_10(%arg0: i16) -> i16 {
    %0 = llvm.mul %arg0, %arg0 : i16
    llvm.return %0 : i16
  }
  llvm.func @lifted_9(%arg0: i16) -> i16 {
    %0 = llvm.add %arg0, %arg0 : i16
    llvm.return %0 : i16
  }
  llvm.func @lifted_8(%arg0: i8) -> i8 {
    %0 = llvm.add %arg0, %arg0 : i8
    %1 = llvm.add %0, %arg0 : i8
    %2 = llvm.mul %1, %arg0 : i8
    llvm.return %2 : i8
  }
  llvm.func @lifted_7(%arg0: i8) -> i8 {
    %0 = llvm.mul %arg0, %arg0 : i8
    %1 = llvm.add %0, %0 : i8
    llvm.return %1 : i8
  }
  llvm.func @lifted_6(%arg0: i8) -> i8 {
    %0 = llvm.mul %arg0, %arg0 : i8
    %1 = llvm.mul %0, %arg0 : i8
    llvm.return %1 : i8
  }
  llvm.func @lifted_5(%arg0: i8) -> i8 {
    %0 = llvm.add %arg0, %arg0 : i8
    %1 = llvm.mul %0, %0 : i8
    llvm.return %1 : i8
  }
  llvm.func @lifted_4(%arg0: i8) -> i8 {
    %0 = llvm.mul %arg0, %arg0 : i8
    %1 = llvm.add %0, %arg0 : i8
    llvm.return %1 : i8
  }
  llvm.func @lifted_3(%arg0: i8) -> i8 {
    %0 = llvm.add %arg0, %arg0 : i8
    %1 = llvm.add %0, %arg0 : i8
    llvm.return %1 : i8
  }
  llvm.func @lifted_2(%arg0: i8) -> i8 {
    %0 = llvm.mul %arg0, %arg0 : i8
    llvm.return %0 : i8
  }
  llvm.func @lifted_1(%arg0: i8) -> i8 {
    %0 = llvm.add %arg0, %arg0 : i8
    llvm.return %0 : i8
  }
  llvm.func @shared_polymorphic_kernel_bank_multitype(%arg0: i8, %arg1: i16, %arg2: i32, %arg3: i64, %arg4: f32, %arg5: f64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i8
    %1 = llvm.mul %arg0, %arg0 : i8
    %2 = llvm.add %0, %arg0 : i8
    %3 = llvm.add %1, %arg0 : i8
    %4 = llvm.mul %0, %0 : i8
    %5 = llvm.mul %1, %arg0 : i8
    %6 = llvm.add %1, %1 : i8
    %7 = llvm.mul %2, %arg0 : i8
    %8 = llvm.add %arg1, %arg1 : i16
    %9 = llvm.mul %arg1, %arg1 : i16
    %10 = llvm.add %8, %arg1 : i16
    %11 = llvm.add %9, %arg1 : i16
    %12 = llvm.mul %8, %8 : i16
    %13 = llvm.mul %9, %arg1 : i16
    %14 = llvm.add %9, %9 : i16
    %15 = llvm.mul %10, %arg1 : i16
    %16 = llvm.add %arg2, %arg2 : i32
    %17 = llvm.mul %arg2, %arg2 : i32
    %18 = llvm.add %16, %arg2 : i32
    %19 = llvm.add %17, %arg2 : i32
    %20 = llvm.mul %16, %16 : i32
    %21 = llvm.mul %17, %arg2 : i32
    %22 = llvm.add %17, %17 : i32
    %23 = llvm.mul %18, %arg2 : i32
    %24 = llvm.add %arg3, %arg3 : i64
    %25 = llvm.mul %arg3, %arg3 : i64
    %26 = llvm.add %24, %arg3 : i64
    %27 = llvm.add %25, %arg3 : i64
    %28 = llvm.mul %24, %24 : i64
    %29 = llvm.mul %25, %arg3 : i64
    %30 = llvm.add %25, %25 : i64
    %31 = llvm.mul %26, %arg3 : i64
    %32 = llvm.fadd %arg4, %arg4 : f32
    %33 = llvm.fmul %arg4, %arg4 : f32
    %34 = llvm.fadd %32, %arg4 : f32
    %35 = llvm.fadd %33, %arg4 : f32
    %36 = llvm.fmul %32, %32 : f32
    %37 = llvm.fmul %33, %arg4 : f32
    %38 = llvm.fadd %33, %33 : f32
    %39 = llvm.fmul %34, %arg4 : f32
    %40 = llvm.fadd %arg5, %arg5 : f64
    %41 = llvm.fmul %arg5, %arg5 : f64
    %42 = llvm.fadd %40, %arg5 : f64
    %43 = llvm.fadd %41, %arg5 : f64
    %44 = llvm.fmul %40, %40 : f64
    %45 = llvm.fmul %41, %arg5 : f64
    %46 = llvm.fadd %41, %41 : f64
    %47 = llvm.fmul %42, %arg5 : f64
    %48 = llvm.sext %0 : i8 to i64
    %49 = llvm.sext %1 : i8 to i64
    %50 = llvm.sext %2 : i8 to i64
    %51 = llvm.sext %3 : i8 to i64
    %52 = llvm.sext %4 : i8 to i64
    %53 = llvm.sext %5 : i8 to i64
    %54 = llvm.sext %6 : i8 to i64
    %55 = llvm.sext %7 : i8 to i64
    %56 = llvm.sext %8 : i16 to i64
    %57 = llvm.sext %9 : i16 to i64
    %58 = llvm.sext %10 : i16 to i64
    %59 = llvm.sext %11 : i16 to i64
    %60 = llvm.sext %12 : i16 to i64
    %61 = llvm.sext %13 : i16 to i64
    %62 = llvm.sext %14 : i16 to i64
    %63 = llvm.sext %15 : i16 to i64
    %64 = llvm.sext %16 : i32 to i64
    %65 = llvm.sext %17 : i32 to i64
    %66 = llvm.sext %18 : i32 to i64
    %67 = llvm.sext %19 : i32 to i64
    %68 = llvm.sext %20 : i32 to i64
    %69 = llvm.sext %21 : i32 to i64
    %70 = llvm.sext %22 : i32 to i64
    %71 = llvm.sext %23 : i32 to i64
    %72 = llvm.fptosi %32 : f32 to i64
    %73 = llvm.fptosi %33 : f32 to i64
    %74 = llvm.fptosi %34 : f32 to i64
    %75 = llvm.fptosi %35 : f32 to i64
    %76 = llvm.fptosi %36 : f32 to i64
    %77 = llvm.fptosi %37 : f32 to i64
    %78 = llvm.fptosi %38 : f32 to i64
    %79 = llvm.fptosi %39 : f32 to i64
    %80 = llvm.fptosi %40 : f64 to i64
    %81 = llvm.fptosi %41 : f64 to i64
    %82 = llvm.fptosi %42 : f64 to i64
    %83 = llvm.fptosi %43 : f64 to i64
    %84 = llvm.fptosi %44 : f64 to i64
    %85 = llvm.fptosi %45 : f64 to i64
    %86 = llvm.fptosi %46 : f64 to i64
    %87 = llvm.fptosi %47 : f64 to i64
    %88 = llvm.add %48, %49 : i64
    %89 = llvm.add %88, %50 : i64
    %90 = llvm.add %89, %51 : i64
    %91 = llvm.add %90, %52 : i64
    %92 = llvm.add %91, %53 : i64
    %93 = llvm.add %92, %54 : i64
    %94 = llvm.add %93, %55 : i64
    %95 = llvm.add %94, %56 : i64
    %96 = llvm.add %95, %57 : i64
    %97 = llvm.add %96, %58 : i64
    %98 = llvm.add %97, %59 : i64
    %99 = llvm.add %98, %60 : i64
    %100 = llvm.add %99, %61 : i64
    %101 = llvm.add %100, %62 : i64
    %102 = llvm.add %101, %63 : i64
    %103 = llvm.add %102, %64 : i64
    %104 = llvm.add %103, %65 : i64
    %105 = llvm.add %104, %66 : i64
    %106 = llvm.add %105, %67 : i64
    %107 = llvm.add %106, %68 : i64
    %108 = llvm.add %107, %69 : i64
    %109 = llvm.add %108, %70 : i64
    %110 = llvm.add %109, %71 : i64
    %111 = llvm.add %110, %24 : i64
    %112 = llvm.add %111, %25 : i64
    %113 = llvm.add %112, %26 : i64
    %114 = llvm.add %113, %27 : i64
    %115 = llvm.add %114, %28 : i64
    %116 = llvm.add %115, %29 : i64
    %117 = llvm.add %116, %30 : i64
    %118 = llvm.add %117, %31 : i64
    %119 = llvm.add %118, %72 : i64
    %120 = llvm.add %119, %73 : i64
    %121 = llvm.add %120, %74 : i64
    %122 = llvm.add %121, %75 : i64
    %123 = llvm.add %122, %76 : i64
    %124 = llvm.add %123, %77 : i64
    %125 = llvm.add %124, %78 : i64
    %126 = llvm.add %125, %79 : i64
    %127 = llvm.add %126, %80 : i64
    %128 = llvm.add %127, %81 : i64
    %129 = llvm.add %128, %82 : i64
    %130 = llvm.add %129, %83 : i64
    %131 = llvm.add %130, %84 : i64
    %132 = llvm.add %131, %85 : i64
    %133 = llvm.add %132, %86 : i64
    %134 = llvm.add %133, %87 : i64
    llvm.return %134 : i64
  }
}


module {
  llvm.func @int_k1_i8(%arg0: i8) -> i8 {
    %0 = llvm.add %arg0, %arg0 : i8
    llvm.return %0 : i8
  }
  llvm.func @int_k2_i8(%arg0: i8) -> i8 {
    %0 = llvm.mul %arg0, %arg0 : i8
    llvm.return %0 : i8
  }
  llvm.func @int_k3_i8(%arg0: i8) -> i8 {
    %0 = llvm.add %arg0, %arg0 : i8
    %1 = llvm.add %0, %arg0 : i8
    llvm.return %1 : i8
  }
  llvm.func @int_k4_i8(%arg0: i8) -> i8 {
    %0 = llvm.mul %arg0, %arg0 : i8
    %1 = llvm.add %0, %arg0 : i8
    llvm.return %1 : i8
  }
  llvm.func @int_k5_i8(%arg0: i8) -> i8 {
    %0 = llvm.add %arg0, %arg0 : i8
    %1 = llvm.mul %0, %0 : i8
    llvm.return %1 : i8
  }
  llvm.func @int_k6_i8(%arg0: i8) -> i8 {
    %0 = llvm.mul %arg0, %arg0 : i8
    %1 = llvm.mul %0, %arg0 : i8
    llvm.return %1 : i8
  }
  llvm.func @int_k7_i8(%arg0: i8) -> i8 {
    %0 = llvm.mul %arg0, %arg0 : i8
    %1 = llvm.add %0, %0 : i8
    llvm.return %1 : i8
  }
  llvm.func @int_k8_i8(%arg0: i8) -> i8 {
    %0 = llvm.add %arg0, %arg0 : i8
    %1 = llvm.add %0, %arg0 : i8
    %2 = llvm.mul %1, %arg0 : i8
    llvm.return %2 : i8
  }
  llvm.func @int_k1_i16(%arg0: i16) -> i16 {
    %0 = llvm.add %arg0, %arg0 : i16
    llvm.return %0 : i16
  }
  llvm.func @int_k2_i16(%arg0: i16) -> i16 {
    %0 = llvm.mul %arg0, %arg0 : i16
    llvm.return %0 : i16
  }
  llvm.func @int_k3_i16(%arg0: i16) -> i16 {
    %0 = llvm.add %arg0, %arg0 : i16
    %1 = llvm.add %0, %arg0 : i16
    llvm.return %1 : i16
  }
  llvm.func @int_k4_i16(%arg0: i16) -> i16 {
    %0 = llvm.mul %arg0, %arg0 : i16
    %1 = llvm.add %0, %arg0 : i16
    llvm.return %1 : i16
  }
  llvm.func @int_k5_i16(%arg0: i16) -> i16 {
    %0 = llvm.add %arg0, %arg0 : i16
    %1 = llvm.mul %0, %0 : i16
    llvm.return %1 : i16
  }
  llvm.func @int_k6_i16(%arg0: i16) -> i16 {
    %0 = llvm.mul %arg0, %arg0 : i16
    %1 = llvm.mul %0, %arg0 : i16
    llvm.return %1 : i16
  }
  llvm.func @int_k7_i16(%arg0: i16) -> i16 {
    %0 = llvm.mul %arg0, %arg0 : i16
    %1 = llvm.add %0, %0 : i16
    llvm.return %1 : i16
  }
  llvm.func @int_k8_i16(%arg0: i16) -> i16 {
    %0 = llvm.add %arg0, %arg0 : i16
    %1 = llvm.add %0, %arg0 : i16
    %2 = llvm.mul %1, %arg0 : i16
    llvm.return %2 : i16
  }
  llvm.func @int_k1_i32(%arg0: i32) -> i32 {
    %0 = llvm.add %arg0, %arg0 : i32
    llvm.return %0 : i32
  }
  llvm.func @int_k2_i32(%arg0: i32) -> i32 {
    %0 = llvm.mul %arg0, %arg0 : i32
    llvm.return %0 : i32
  }
  llvm.func @int_k3_i32(%arg0: i32) -> i32 {
    %0 = llvm.add %arg0, %arg0 : i32
    %1 = llvm.add %0, %arg0 : i32
    llvm.return %1 : i32
  }
  llvm.func @int_k4_i32(%arg0: i32) -> i32 {
    %0 = llvm.mul %arg0, %arg0 : i32
    %1 = llvm.add %0, %arg0 : i32
    llvm.return %1 : i32
  }
  llvm.func @int_k5_i32(%arg0: i32) -> i32 {
    %0 = llvm.add %arg0, %arg0 : i32
    %1 = llvm.mul %0, %0 : i32
    llvm.return %1 : i32
  }
  llvm.func @int_k6_i32(%arg0: i32) -> i32 {
    %0 = llvm.mul %arg0, %arg0 : i32
    %1 = llvm.mul %0, %arg0 : i32
    llvm.return %1 : i32
  }
  llvm.func @int_k7_i32(%arg0: i32) -> i32 {
    %0 = llvm.mul %arg0, %arg0 : i32
    %1 = llvm.add %0, %0 : i32
    llvm.return %1 : i32
  }
  llvm.func @int_k8_i32(%arg0: i32) -> i32 {
    %0 = llvm.add %arg0, %arg0 : i32
    %1 = llvm.add %0, %arg0 : i32
    %2 = llvm.mul %1, %arg0 : i32
    llvm.return %2 : i32
  }
  llvm.func @int_k1_i64(%arg0: i64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i64
    llvm.return %0 : i64
  }
  llvm.func @int_k2_i64(%arg0: i64) -> i64 {
    %0 = llvm.mul %arg0, %arg0 : i64
    llvm.return %0 : i64
  }
  llvm.func @int_k3_i64(%arg0: i64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i64
    %1 = llvm.add %0, %arg0 : i64
    llvm.return %1 : i64
  }
  llvm.func @int_k4_i64(%arg0: i64) -> i64 {
    %0 = llvm.mul %arg0, %arg0 : i64
    %1 = llvm.add %0, %arg0 : i64
    llvm.return %1 : i64
  }
  llvm.func @int_k5_i64(%arg0: i64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i64
    %1 = llvm.mul %0, %0 : i64
    llvm.return %1 : i64
  }
  llvm.func @int_k6_i64(%arg0: i64) -> i64 {
    %0 = llvm.mul %arg0, %arg0 : i64
    %1 = llvm.mul %0, %arg0 : i64
    llvm.return %1 : i64
  }
  llvm.func @int_k7_i64(%arg0: i64) -> i64 {
    %0 = llvm.mul %arg0, %arg0 : i64
    %1 = llvm.add %0, %0 : i64
    llvm.return %1 : i64
  }
  llvm.func @int_k8_i64(%arg0: i64) -> i64 {
    %0 = llvm.add %arg0, %arg0 : i64
    %1 = llvm.add %0, %arg0 : i64
    %2 = llvm.mul %1, %arg0 : i64
    llvm.return %2 : i64
  }
  llvm.func @float_k1_f32(%arg0: f32) -> f32 {
    %0 = llvm.fadd %arg0, %arg0 : f32
    llvm.return %0 : f32
  }
  llvm.func @float_k2_f32(%arg0: f32) -> f32 {
    %0 = llvm.fmul %arg0, %arg0 : f32
    llvm.return %0 : f32
  }
  llvm.func @float_k3_f32(%arg0: f32) -> f32 {
    %0 = llvm.fadd %arg0, %arg0 : f32
    %1 = llvm.fadd %0, %arg0 : f32
    llvm.return %1 : f32
  }
  llvm.func @float_k4_f32(%arg0: f32) -> f32 {
    %0 = llvm.fmul %arg0, %arg0 : f32
    %1 = llvm.fadd %0, %arg0 : f32
    llvm.return %1 : f32
  }
  llvm.func @float_k5_f32(%arg0: f32) -> f32 {
    %0 = llvm.fadd %arg0, %arg0 : f32
    %1 = llvm.fmul %0, %0 : f32
    llvm.return %1 : f32
  }
  llvm.func @float_k6_f32(%arg0: f32) -> f32 {
    %0 = llvm.fmul %arg0, %arg0 : f32
    %1 = llvm.fmul %0, %arg0 : f32
    llvm.return %1 : f32
  }
  llvm.func @float_k7_f32(%arg0: f32) -> f32 {
    %0 = llvm.fmul %arg0, %arg0 : f32
    %1 = llvm.fadd %0, %0 : f32
    llvm.return %1 : f32
  }
  llvm.func @float_k8_f32(%arg0: f32) -> f32 {
    %0 = llvm.fadd %arg0, %arg0 : f32
    %1 = llvm.fadd %0, %arg0 : f32
    %2 = llvm.fmul %1, %arg0 : f32
    llvm.return %2 : f32
  }
  llvm.func @float_k1_f64(%arg0: f64) -> f64 {
    %0 = llvm.fadd %arg0, %arg0 : f64
    llvm.return %0 : f64
  }
  llvm.func @float_k2_f64(%arg0: f64) -> f64 {
    %0 = llvm.fmul %arg0, %arg0 : f64
    llvm.return %0 : f64
  }
  llvm.func @float_k3_f64(%arg0: f64) -> f64 {
    %0 = llvm.fadd %arg0, %arg0 : f64
    %1 = llvm.fadd %0, %arg0 : f64
    llvm.return %1 : f64
  }
  llvm.func @float_k4_f64(%arg0: f64) -> f64 {
    %0 = llvm.fmul %arg0, %arg0 : f64
    %1 = llvm.fadd %0, %arg0 : f64
    llvm.return %1 : f64
  }
  llvm.func @float_k5_f64(%arg0: f64) -> f64 {
    %0 = llvm.fadd %arg0, %arg0 : f64
    %1 = llvm.fmul %0, %0 : f64
    llvm.return %1 : f64
  }
  llvm.func @float_k6_f64(%arg0: f64) -> f64 {
    %0 = llvm.fmul %arg0, %arg0 : f64
    %1 = llvm.fmul %0, %arg0 : f64
    llvm.return %1 : f64
  }
  llvm.func @float_k7_f64(%arg0: f64) -> f64 {
    %0 = llvm.fmul %arg0, %arg0 : f64
    %1 = llvm.fadd %0, %0 : f64
    llvm.return %1 : f64
  }
  llvm.func @float_k8_f64(%arg0: f64) -> f64 {
    %0 = llvm.fadd %arg0, %arg0 : f64
    %1 = llvm.fadd %0, %arg0 : f64
    %2 = llvm.fmul %1, %arg0 : f64
    llvm.return %2 : f64
  }
  llvm.func @shared_polymorphic_kernel_bank_multitype(%arg0: i8, %arg1: i16, %arg2: i32, %arg3: i64, %arg4: f32, %arg5: f64) -> i64 {
    %0 = llvm.call @int_k1_i8(%arg0) : (i8) -> i8
    %1 = llvm.call @int_k2_i8(%arg0) : (i8) -> i8
    %2 = llvm.call @int_k3_i8(%arg0) : (i8) -> i8
    %3 = llvm.call @int_k4_i8(%arg0) : (i8) -> i8
    %4 = llvm.call @int_k5_i8(%arg0) : (i8) -> i8
    %5 = llvm.call @int_k6_i8(%arg0) : (i8) -> i8
    %6 = llvm.call @int_k7_i8(%arg0) : (i8) -> i8
    %7 = llvm.call @int_k8_i8(%arg0) : (i8) -> i8
    %8 = llvm.call @int_k1_i16(%arg1) : (i16) -> i16
    %9 = llvm.call @int_k2_i16(%arg1) : (i16) -> i16
    %10 = llvm.call @int_k3_i16(%arg1) : (i16) -> i16
    %11 = llvm.call @int_k4_i16(%arg1) : (i16) -> i16
    %12 = llvm.call @int_k5_i16(%arg1) : (i16) -> i16
    %13 = llvm.call @int_k6_i16(%arg1) : (i16) -> i16
    %14 = llvm.call @int_k7_i16(%arg1) : (i16) -> i16
    %15 = llvm.call @int_k8_i16(%arg1) : (i16) -> i16
    %16 = llvm.call @int_k1_i32(%arg2) : (i32) -> i32
    %17 = llvm.call @int_k2_i32(%arg2) : (i32) -> i32
    %18 = llvm.call @int_k3_i32(%arg2) : (i32) -> i32
    %19 = llvm.call @int_k4_i32(%arg2) : (i32) -> i32
    %20 = llvm.call @int_k5_i32(%arg2) : (i32) -> i32
    %21 = llvm.call @int_k6_i32(%arg2) : (i32) -> i32
    %22 = llvm.call @int_k7_i32(%arg2) : (i32) -> i32
    %23 = llvm.call @int_k8_i32(%arg2) : (i32) -> i32
    %24 = llvm.call @int_k1_i64(%arg3) : (i64) -> i64
    %25 = llvm.call @int_k2_i64(%arg3) : (i64) -> i64
    %26 = llvm.call @int_k3_i64(%arg3) : (i64) -> i64
    %27 = llvm.call @int_k4_i64(%arg3) : (i64) -> i64
    %28 = llvm.call @int_k5_i64(%arg3) : (i64) -> i64
    %29 = llvm.call @int_k6_i64(%arg3) : (i64) -> i64
    %30 = llvm.call @int_k7_i64(%arg3) : (i64) -> i64
    %31 = llvm.call @int_k8_i64(%arg3) : (i64) -> i64
    %32 = llvm.call @float_k1_f32(%arg4) : (f32) -> f32
    %33 = llvm.call @float_k2_f32(%arg4) : (f32) -> f32
    %34 = llvm.call @float_k3_f32(%arg4) : (f32) -> f32
    %35 = llvm.call @float_k4_f32(%arg4) : (f32) -> f32
    %36 = llvm.call @float_k5_f32(%arg4) : (f32) -> f32
    %37 = llvm.call @float_k6_f32(%arg4) : (f32) -> f32
    %38 = llvm.call @float_k7_f32(%arg4) : (f32) -> f32
    %39 = llvm.call @float_k8_f32(%arg4) : (f32) -> f32
    %40 = llvm.call @float_k1_f64(%arg5) : (f64) -> f64
    %41 = llvm.call @float_k2_f64(%arg5) : (f64) -> f64
    %42 = llvm.call @float_k3_f64(%arg5) : (f64) -> f64
    %43 = llvm.call @float_k4_f64(%arg5) : (f64) -> f64
    %44 = llvm.call @float_k5_f64(%arg5) : (f64) -> f64
    %45 = llvm.call @float_k6_f64(%arg5) : (f64) -> f64
    %46 = llvm.call @float_k7_f64(%arg5) : (f64) -> f64
    %47 = llvm.call @float_k8_f64(%arg5) : (f64) -> f64
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


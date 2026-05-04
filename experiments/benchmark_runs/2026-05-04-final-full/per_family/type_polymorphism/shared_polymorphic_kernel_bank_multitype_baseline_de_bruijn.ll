; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define double @lifted_48(double %0) {
  %2 = fadd double %0, %0
  %3 = fadd double %2, %0
  %4 = fmul double %3, %0
  ret double %4
}

define double @lifted_47(double %0) {
  %2 = fmul double %0, %0
  %3 = fadd double %2, %2
  ret double %3
}

define double @lifted_46(double %0) {
  %2 = fmul double %0, %0
  %3 = fmul double %2, %0
  ret double %3
}

define double @lifted_45(double %0) {
  %2 = fadd double %0, %0
  %3 = fmul double %2, %2
  ret double %3
}

define double @lifted_44(double %0) {
  %2 = fmul double %0, %0
  %3 = fadd double %2, %0
  ret double %3
}

define double @lifted_43(double %0) {
  %2 = fadd double %0, %0
  %3 = fadd double %2, %0
  ret double %3
}

define double @lifted_42(double %0) {
  %2 = fmul double %0, %0
  ret double %2
}

define double @lifted_41(double %0) {
  %2 = fadd double %0, %0
  ret double %2
}

define float @lifted_40(float %0) {
  %2 = fadd float %0, %0
  %3 = fadd float %2, %0
  %4 = fmul float %3, %0
  ret float %4
}

define float @lifted_39(float %0) {
  %2 = fmul float %0, %0
  %3 = fadd float %2, %2
  ret float %3
}

define float @lifted_38(float %0) {
  %2 = fmul float %0, %0
  %3 = fmul float %2, %0
  ret float %3
}

define float @lifted_37(float %0) {
  %2 = fadd float %0, %0
  %3 = fmul float %2, %2
  ret float %3
}

define float @lifted_36(float %0) {
  %2 = fmul float %0, %0
  %3 = fadd float %2, %0
  ret float %3
}

define float @lifted_35(float %0) {
  %2 = fadd float %0, %0
  %3 = fadd float %2, %0
  ret float %3
}

define float @lifted_34(float %0) {
  %2 = fmul float %0, %0
  ret float %2
}

define float @lifted_33(float %0) {
  %2 = fadd float %0, %0
  ret float %2
}

define i64 @lifted_32(i64 %0) {
  %2 = add i64 %0, %0
  %3 = add i64 %2, %0
  %4 = mul i64 %3, %0
  ret i64 %4
}

define i64 @lifted_31(i64 %0) {
  %2 = mul i64 %0, %0
  %3 = add i64 %2, %2
  ret i64 %3
}

define i64 @lifted_30(i64 %0) {
  %2 = mul i64 %0, %0
  %3 = mul i64 %2, %0
  ret i64 %3
}

define i64 @lifted_29(i64 %0) {
  %2 = add i64 %0, %0
  %3 = mul i64 %2, %2
  ret i64 %3
}

define i64 @lifted_28(i64 %0) {
  %2 = mul i64 %0, %0
  %3 = add i64 %2, %0
  ret i64 %3
}

define i64 @lifted_27(i64 %0) {
  %2 = add i64 %0, %0
  %3 = add i64 %2, %0
  ret i64 %3
}

define i64 @lifted_26(i64 %0) {
  %2 = mul i64 %0, %0
  ret i64 %2
}

define i64 @lifted_25(i64 %0) {
  %2 = add i64 %0, %0
  ret i64 %2
}

define i32 @lifted_24(i32 %0) {
  %2 = add i32 %0, %0
  %3 = add i32 %2, %0
  %4 = mul i32 %3, %0
  ret i32 %4
}

define i32 @lifted_23(i32 %0) {
  %2 = mul i32 %0, %0
  %3 = add i32 %2, %2
  ret i32 %3
}

define i32 @lifted_22(i32 %0) {
  %2 = mul i32 %0, %0
  %3 = mul i32 %2, %0
  ret i32 %3
}

define i32 @lifted_21(i32 %0) {
  %2 = add i32 %0, %0
  %3 = mul i32 %2, %2
  ret i32 %3
}

define i32 @lifted_20(i32 %0) {
  %2 = mul i32 %0, %0
  %3 = add i32 %2, %0
  ret i32 %3
}

define i32 @lifted_19(i32 %0) {
  %2 = add i32 %0, %0
  %3 = add i32 %2, %0
  ret i32 %3
}

define i32 @lifted_18(i32 %0) {
  %2 = mul i32 %0, %0
  ret i32 %2
}

define i32 @lifted_17(i32 %0) {
  %2 = add i32 %0, %0
  ret i32 %2
}

define i16 @lifted_16(i16 %0) {
  %2 = add i16 %0, %0
  %3 = add i16 %2, %0
  %4 = mul i16 %3, %0
  ret i16 %4
}

define i16 @lifted_15(i16 %0) {
  %2 = mul i16 %0, %0
  %3 = add i16 %2, %2
  ret i16 %3
}

define i16 @lifted_14(i16 %0) {
  %2 = mul i16 %0, %0
  %3 = mul i16 %2, %0
  ret i16 %3
}

define i16 @lifted_13(i16 %0) {
  %2 = add i16 %0, %0
  %3 = mul i16 %2, %2
  ret i16 %3
}

define i16 @lifted_12(i16 %0) {
  %2 = mul i16 %0, %0
  %3 = add i16 %2, %0
  ret i16 %3
}

define i16 @lifted_11(i16 %0) {
  %2 = add i16 %0, %0
  %3 = add i16 %2, %0
  ret i16 %3
}

define i16 @lifted_10(i16 %0) {
  %2 = mul i16 %0, %0
  ret i16 %2
}

define i16 @lifted_9(i16 %0) {
  %2 = add i16 %0, %0
  ret i16 %2
}

define i8 @lifted_8(i8 %0) {
  %2 = add i8 %0, %0
  %3 = add i8 %2, %0
  %4 = mul i8 %3, %0
  ret i8 %4
}

define i8 @lifted_7(i8 %0) {
  %2 = mul i8 %0, %0
  %3 = add i8 %2, %2
  ret i8 %3
}

define i8 @lifted_6(i8 %0) {
  %2 = mul i8 %0, %0
  %3 = mul i8 %2, %0
  ret i8 %3
}

define i8 @lifted_5(i8 %0) {
  %2 = add i8 %0, %0
  %3 = mul i8 %2, %2
  ret i8 %3
}

define i8 @lifted_4(i8 %0) {
  %2 = mul i8 %0, %0
  %3 = add i8 %2, %0
  ret i8 %3
}

define i8 @lifted_3(i8 %0) {
  %2 = add i8 %0, %0
  %3 = add i8 %2, %0
  ret i8 %3
}

define i8 @lifted_2(i8 %0) {
  %2 = mul i8 %0, %0
  ret i8 %2
}

define i8 @lifted_1(i8 %0) {
  %2 = add i8 %0, %0
  ret i8 %2
}

define i64 @shared_polymorphic_kernel_bank_multitype(i8 %0, i16 %1, i32 %2, i64 %3, float %4, double %5) {
  %7 = add i8 %0, %0
  %8 = mul i8 %0, %0
  %9 = add i8 %7, %0
  %10 = add i8 %8, %0
  %11 = mul i8 %7, %7
  %12 = mul i8 %8, %0
  %13 = add i8 %8, %8
  %14 = mul i8 %9, %0
  %15 = add i16 %1, %1
  %16 = mul i16 %1, %1
  %17 = add i16 %15, %1
  %18 = add i16 %16, %1
  %19 = mul i16 %15, %15
  %20 = mul i16 %16, %1
  %21 = add i16 %16, %16
  %22 = mul i16 %17, %1
  %23 = add i32 %2, %2
  %24 = mul i32 %2, %2
  %25 = add i32 %23, %2
  %26 = add i32 %24, %2
  %27 = mul i32 %23, %23
  %28 = mul i32 %24, %2
  %29 = add i32 %24, %24
  %30 = mul i32 %25, %2
  %31 = add i64 %3, %3
  %32 = mul i64 %3, %3
  %33 = add i64 %31, %3
  %34 = add i64 %32, %3
  %35 = mul i64 %31, %31
  %36 = mul i64 %32, %3
  %37 = add i64 %32, %32
  %38 = mul i64 %33, %3
  %39 = fadd float %4, %4
  %40 = fmul float %4, %4
  %41 = fadd float %39, %4
  %42 = fadd float %40, %4
  %43 = fmul float %39, %39
  %44 = fmul float %40, %4
  %45 = fadd float %40, %40
  %46 = fmul float %41, %4
  %47 = fadd double %5, %5
  %48 = fmul double %5, %5
  %49 = fadd double %47, %5
  %50 = fadd double %48, %5
  %51 = fmul double %47, %47
  %52 = fmul double %48, %5
  %53 = fadd double %48, %48
  %54 = fmul double %49, %5
  %55 = sext i8 %7 to i64
  %56 = sext i8 %8 to i64
  %57 = sext i8 %9 to i64
  %58 = sext i8 %10 to i64
  %59 = sext i8 %11 to i64
  %60 = sext i8 %12 to i64
  %61 = sext i8 %13 to i64
  %62 = sext i8 %14 to i64
  %63 = sext i16 %15 to i64
  %64 = sext i16 %16 to i64
  %65 = sext i16 %17 to i64
  %66 = sext i16 %18 to i64
  %67 = sext i16 %19 to i64
  %68 = sext i16 %20 to i64
  %69 = sext i16 %21 to i64
  %70 = sext i16 %22 to i64
  %71 = sext i32 %23 to i64
  %72 = sext i32 %24 to i64
  %73 = sext i32 %25 to i64
  %74 = sext i32 %26 to i64
  %75 = sext i32 %27 to i64
  %76 = sext i32 %28 to i64
  %77 = sext i32 %29 to i64
  %78 = sext i32 %30 to i64
  %79 = fptosi float %39 to i64
  %80 = fptosi float %40 to i64
  %81 = fptosi float %41 to i64
  %82 = fptosi float %42 to i64
  %83 = fptosi float %43 to i64
  %84 = fptosi float %44 to i64
  %85 = fptosi float %45 to i64
  %86 = fptosi float %46 to i64
  %87 = fptosi double %47 to i64
  %88 = fptosi double %48 to i64
  %89 = fptosi double %49 to i64
  %90 = fptosi double %50 to i64
  %91 = fptosi double %51 to i64
  %92 = fptosi double %52 to i64
  %93 = fptosi double %53 to i64
  %94 = fptosi double %54 to i64
  %95 = add i64 %55, %56
  %96 = add i64 %95, %57
  %97 = add i64 %96, %58
  %98 = add i64 %97, %59
  %99 = add i64 %98, %60
  %100 = add i64 %99, %61
  %101 = add i64 %100, %62
  %102 = add i64 %101, %63
  %103 = add i64 %102, %64
  %104 = add i64 %103, %65
  %105 = add i64 %104, %66
  %106 = add i64 %105, %67
  %107 = add i64 %106, %68
  %108 = add i64 %107, %69
  %109 = add i64 %108, %70
  %110 = add i64 %109, %71
  %111 = add i64 %110, %72
  %112 = add i64 %111, %73
  %113 = add i64 %112, %74
  %114 = add i64 %113, %75
  %115 = add i64 %114, %76
  %116 = add i64 %115, %77
  %117 = add i64 %116, %78
  %118 = add i64 %117, %31
  %119 = add i64 %118, %32
  %120 = add i64 %119, %33
  %121 = add i64 %120, %34
  %122 = add i64 %121, %35
  %123 = add i64 %122, %36
  %124 = add i64 %123, %37
  %125 = add i64 %124, %38
  %126 = add i64 %125, %79
  %127 = add i64 %126, %80
  %128 = add i64 %127, %81
  %129 = add i64 %128, %82
  %130 = add i64 %129, %83
  %131 = add i64 %130, %84
  %132 = add i64 %131, %85
  %133 = add i64 %132, %86
  %134 = add i64 %133, %87
  %135 = add i64 %134, %88
  %136 = add i64 %135, %89
  %137 = add i64 %136, %90
  %138 = add i64 %137, %91
  %139 = add i64 %138, %92
  %140 = add i64 %139, %93
  %141 = add i64 %140, %94
  ret i64 %141
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

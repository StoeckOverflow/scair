; ModuleID = '/home/dominic/dev/scair/experiments/type_polymorphism/out/shared_polymorphic_kernel_bank_multitype_value_dependent.ll'
source_filename = "LLVMDialectModule"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @lifted_48(double %0) local_unnamed_addr #0 {
  %2 = fadd double %0, %0
  %3 = fadd double %0, %2
  %4 = fmul double %0, %3
  ret double %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @lifted_47(double %0) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  %3 = fadd double %2, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @lifted_46(double %0) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  %3 = fmul double %0, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @lifted_45(double %0) local_unnamed_addr #0 {
  %2 = fadd double %0, %0
  %3 = fmul double %2, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @lifted_44(double %0) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  %3 = fadd double %0, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @lifted_43(double %0) local_unnamed_addr #0 {
  %2 = fadd double %0, %0
  %3 = fadd double %0, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @lifted_42(double %0) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  ret double %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @lifted_41(double %0) local_unnamed_addr #0 {
  %2 = fadd double %0, %0
  ret double %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @lifted_40(float %0) local_unnamed_addr #0 {
  %2 = fadd float %0, %0
  %3 = fadd float %0, %2
  %4 = fmul float %0, %3
  ret float %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @lifted_39(float %0) local_unnamed_addr #0 {
  %2 = fmul float %0, %0
  %3 = fadd float %2, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @lifted_38(float %0) local_unnamed_addr #0 {
  %2 = fmul float %0, %0
  %3 = fmul float %0, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @lifted_37(float %0) local_unnamed_addr #0 {
  %2 = fadd float %0, %0
  %3 = fmul float %2, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @lifted_36(float %0) local_unnamed_addr #0 {
  %2 = fmul float %0, %0
  %3 = fadd float %0, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @lifted_35(float %0) local_unnamed_addr #0 {
  %2 = fadd float %0, %0
  %3 = fadd float %0, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @lifted_34(float %0) local_unnamed_addr #0 {
  %2 = fmul float %0, %0
  ret float %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @lifted_33(float %0) local_unnamed_addr #0 {
  %2 = fadd float %0, %0
  ret float %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @lifted_32(i64 %0) local_unnamed_addr #0 {
  %2 = mul i64 %0, 3
  %3 = mul i64 %2, %0
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i64 0, -1) i64 @lifted_31(i64 %0) local_unnamed_addr #0 {
  %2 = shl i64 %0, 1
  %3 = mul i64 %2, %0
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @lifted_30(i64 %0) local_unnamed_addr #0 {
  %2 = mul i64 %0, %0
  %3 = mul i64 %2, %0
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @lifted_29(i64 %0) local_unnamed_addr #0 {
  %2 = shl i64 %0, 1
  %3 = mul i64 %2, %2
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @lifted_28(i64 %0) local_unnamed_addr #0 {
  %2 = add i64 %0, 1
  %3 = mul i64 %2, %0
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @lifted_27(i64 %0) local_unnamed_addr #0 {
  %2 = mul i64 %0, 3
  ret i64 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @lifted_26(i64 %0) local_unnamed_addr #0 {
  %2 = mul i64 %0, %0
  ret i64 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i64 0, -1) i64 @lifted_25(i64 %0) local_unnamed_addr #0 {
  %2 = shl i64 %0, 1
  ret i64 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @lifted_24(i32 %0) local_unnamed_addr #0 {
  %2 = mul i32 %0, 3
  %3 = mul i32 %2, %0
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i32 0, -1) i32 @lifted_23(i32 %0) local_unnamed_addr #0 {
  %2 = shl i32 %0, 1
  %3 = mul i32 %2, %0
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @lifted_22(i32 %0) local_unnamed_addr #0 {
  %2 = mul i32 %0, %0
  %3 = mul i32 %2, %0
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @lifted_21(i32 %0) local_unnamed_addr #0 {
  %2 = shl i32 %0, 1
  %3 = mul i32 %2, %2
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @lifted_20(i32 %0) local_unnamed_addr #0 {
  %2 = add i32 %0, 1
  %3 = mul i32 %2, %0
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @lifted_19(i32 %0) local_unnamed_addr #0 {
  %2 = mul i32 %0, 3
  ret i32 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @lifted_18(i32 %0) local_unnamed_addr #0 {
  %2 = mul i32 %0, %0
  ret i32 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i32 0, -1) i32 @lifted_17(i32 %0) local_unnamed_addr #0 {
  %2 = shl i32 %0, 1
  ret i32 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @lifted_16(i16 %0) local_unnamed_addr #0 {
  %2 = mul i16 %0, 3
  %3 = mul i16 %2, %0
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i16 0, -1) i16 @lifted_15(i16 %0) local_unnamed_addr #0 {
  %2 = shl i16 %0, 1
  %3 = mul i16 %2, %0
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @lifted_14(i16 %0) local_unnamed_addr #0 {
  %2 = mul i16 %0, %0
  %3 = mul i16 %2, %0
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @lifted_13(i16 %0) local_unnamed_addr #0 {
  %2 = shl i16 %0, 1
  %3 = mul i16 %2, %2
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @lifted_12(i16 %0) local_unnamed_addr #0 {
  %2 = add i16 %0, 1
  %3 = mul i16 %2, %0
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @lifted_11(i16 %0) local_unnamed_addr #0 {
  %2 = mul i16 %0, 3
  ret i16 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @lifted_10(i16 %0) local_unnamed_addr #0 {
  %2 = mul i16 %0, %0
  ret i16 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i16 0, -1) i16 @lifted_9(i16 %0) local_unnamed_addr #0 {
  %2 = shl i16 %0, 1
  ret i16 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @lifted_8(i8 %0) local_unnamed_addr #0 {
  %2 = mul i8 %0, 3
  %3 = mul i8 %2, %0
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i8 0, -1) i8 @lifted_7(i8 %0) local_unnamed_addr #0 {
  %2 = shl i8 %0, 1
  %3 = mul i8 %2, %0
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @lifted_6(i8 %0) local_unnamed_addr #0 {
  %2 = mul i8 %0, %0
  %3 = mul i8 %2, %0
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @lifted_5(i8 %0) local_unnamed_addr #0 {
  %2 = shl i8 %0, 1
  %3 = mul i8 %2, %2
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @lifted_4(i8 %0) local_unnamed_addr #0 {
  %2 = add i8 %0, 1
  %3 = mul i8 %2, %0
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @lifted_3(i8 %0) local_unnamed_addr #0 {
  %2 = mul i8 %0, 3
  ret i8 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @lifted_2(i8 %0) local_unnamed_addr #0 {
  %2 = mul i8 %0, %0
  ret i8 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i8 0, -1) i8 @lifted_1(i8 %0) local_unnamed_addr #0 {
  %2 = shl i8 %0, 1
  ret i8 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @shared_polymorphic_kernel_bank_multitype(i8 %0, i16 %1, i32 %2, i64 %3, float %4, double %5) local_unnamed_addr #0 {
  %7 = shl i8 %0, 1
  %8 = mul i8 %0, %0
  %9 = mul i8 %0, 3
  %10 = add i8 %8, %0
  %11 = mul i8 %7, %7
  %12 = mul i8 %8, %0
  %13 = shl i8 %8, 1
  %14 = mul i8 %9, %0
  %15 = shl i16 %1, 1
  %16 = mul i16 %1, %1
  %17 = mul i16 %1, 3
  %18 = add i16 %16, %1
  %19 = mul i16 %15, %15
  %20 = mul i16 %16, %1
  %21 = shl i16 %16, 1
  %22 = mul i16 %17, %1
  %23 = shl i32 %2, 1
  %24 = mul i32 %2, %2
  %25 = mul i32 %2, 3
  %26 = add i32 %24, %2
  %27 = mul i32 %23, %23
  %28 = mul i32 %24, %2
  %29 = shl i32 %24, 1
  %30 = mul i32 %25, %2
  %31 = shl i64 %3, 1
  %32 = mul i64 %3, %3
  %33 = mul i64 %3, 3
  %34 = mul i64 %31, %31
  %35 = fadd float %4, %4
  %36 = fmul float %4, %4
  %37 = fadd float %4, %35
  %38 = fadd float %4, %36
  %39 = fmul float %35, %35
  %40 = fmul float %4, %36
  %41 = fadd float %36, %36
  %42 = fmul float %4, %37
  %43 = fadd double %5, %5
  %44 = fmul double %5, %5
  %45 = fadd double %5, %43
  %46 = fadd double %5, %44
  %47 = fmul double %43, %43
  %48 = fmul double %5, %44
  %49 = fadd double %44, %44
  %50 = fmul double %5, %45
  %51 = sext i8 %7 to i64
  %52 = sext i8 %8 to i64
  %53 = sext i8 %9 to i64
  %54 = sext i8 %10 to i64
  %55 = sext i8 %11 to i64
  %56 = sext i8 %12 to i64
  %57 = sext i8 %13 to i64
  %58 = sext i8 %14 to i64
  %59 = sext i16 %15 to i64
  %60 = sext i16 %16 to i64
  %61 = sext i16 %17 to i64
  %62 = sext i16 %18 to i64
  %63 = sext i16 %19 to i64
  %64 = sext i16 %20 to i64
  %65 = sext i16 %21 to i64
  %66 = sext i16 %22 to i64
  %67 = sext i32 %23 to i64
  %68 = sext i32 %24 to i64
  %69 = sext i32 %25 to i64
  %70 = sext i32 %26 to i64
  %71 = sext i32 %27 to i64
  %72 = sext i32 %28 to i64
  %73 = sext i32 %29 to i64
  %74 = sext i32 %30 to i64
  %75 = fptosi float %35 to i64
  %76 = fptosi float %36 to i64
  %77 = fptosi float %37 to i64
  %78 = fptosi float %38 to i64
  %79 = fptosi float %39 to i64
  %80 = fptosi float %40 to i64
  %81 = fptosi float %41 to i64
  %82 = fptosi float %42 to i64
  %83 = fptosi double %43 to i64
  %84 = fptosi double %44 to i64
  %85 = fptosi double %45 to i64
  %86 = fptosi double %46 to i64
  %87 = fptosi double %47 to i64
  %88 = fptosi double %48 to i64
  %89 = fptosi double %49 to i64
  %90 = fptosi double %50 to i64
  %reass.add = add i64 %33, %32
  %reass.mul = mul i64 %reass.add, %3
  %reass.mul2 = shl i64 %32, 2
  %91 = add nsw i64 %51, %52
  %92 = add nsw i64 %91, %53
  %93 = add i64 %92, %3
  %94 = add i64 %93, %54
  %95 = add i64 %94, %55
  %96 = add i64 %95, %56
  %97 = add i64 %96, %57
  %98 = add i64 %97, %58
  %99 = add i64 %98, %59
  %100 = add i64 %99, %60
  %101 = add i64 %100, %61
  %102 = add i64 %101, %62
  %103 = add i64 %102, %63
  %104 = add i64 %103, %64
  %105 = add i64 %104, %65
  %106 = add i64 %105, %66
  %107 = add i64 %106, %67
  %108 = add i64 %107, %68
  %109 = add i64 %108, %69
  %110 = add i64 %109, %31
  %111 = add i64 %110, %33
  %112 = add i64 %111, %70
  %113 = add i64 %112, %71
  %114 = add i64 %113, %72
  %115 = add i64 %114, %73
  %116 = add i64 %115, %74
  %117 = add i64 %116, %34
  %118 = add i64 %117, %75
  %119 = add i64 %118, %76
  %120 = add i64 %119, %reass.mul
  %121 = add i64 %120, %reass.mul2
  %122 = add i64 %121, %77
  %123 = add i64 %122, %78
  %124 = add i64 %123, %79
  %125 = add i64 %124, %80
  %126 = add i64 %125, %81
  %127 = add i64 %126, %83
  %128 = add i64 %127, %84
  %129 = add i64 %128, %82
  %130 = add i64 %129, %85
  %131 = add i64 %130, %86
  %132 = add i64 %131, %87
  %133 = add i64 %132, %88
  %134 = add i64 %133, %89
  %135 = add i64 %134, %90
  ret i64 %135
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

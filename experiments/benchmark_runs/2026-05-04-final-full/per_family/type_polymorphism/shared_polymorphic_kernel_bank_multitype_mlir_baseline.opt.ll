; ModuleID = '/home/dominic/dev/scair/experiments/type_polymorphism/out/shared_polymorphic_kernel_bank_multitype_mlir_baseline.ll'
source_filename = "LLVMDialectModule"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i8 0, -1) i8 @int_k1_i8(i8 %0) local_unnamed_addr #0 {
  %2 = shl i8 %0, 1
  ret i8 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @int_k2_i8(i8 %0) local_unnamed_addr #0 {
  %2 = mul i8 %0, %0
  ret i8 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @int_k3_i8(i8 %0) local_unnamed_addr #0 {
  %2 = mul i8 %0, 3
  ret i8 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @int_k4_i8(i8 %0) local_unnamed_addr #0 {
  %2 = add i8 %0, 1
  %3 = mul i8 %2, %0
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @int_k5_i8(i8 %0) local_unnamed_addr #0 {
  %2 = shl i8 %0, 1
  %3 = mul i8 %2, %2
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @int_k6_i8(i8 %0) local_unnamed_addr #0 {
  %2 = mul i8 %0, %0
  %3 = mul i8 %2, %0
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i8 0, -1) i8 @int_k7_i8(i8 %0) local_unnamed_addr #0 {
  %2 = shl i8 %0, 1
  %3 = mul i8 %2, %0
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @int_k8_i8(i8 %0) local_unnamed_addr #0 {
  %2 = mul i8 %0, 3
  %3 = mul i8 %2, %0
  ret i8 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i16 0, -1) i16 @int_k1_i16(i16 %0) local_unnamed_addr #0 {
  %2 = shl i16 %0, 1
  ret i16 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @int_k2_i16(i16 %0) local_unnamed_addr #0 {
  %2 = mul i16 %0, %0
  ret i16 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @int_k3_i16(i16 %0) local_unnamed_addr #0 {
  %2 = mul i16 %0, 3
  ret i16 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @int_k4_i16(i16 %0) local_unnamed_addr #0 {
  %2 = add i16 %0, 1
  %3 = mul i16 %2, %0
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @int_k5_i16(i16 %0) local_unnamed_addr #0 {
  %2 = shl i16 %0, 1
  %3 = mul i16 %2, %2
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @int_k6_i16(i16 %0) local_unnamed_addr #0 {
  %2 = mul i16 %0, %0
  %3 = mul i16 %2, %0
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i16 0, -1) i16 @int_k7_i16(i16 %0) local_unnamed_addr #0 {
  %2 = shl i16 %0, 1
  %3 = mul i16 %2, %0
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @int_k8_i16(i16 %0) local_unnamed_addr #0 {
  %2 = mul i16 %0, 3
  %3 = mul i16 %2, %0
  ret i16 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i32 0, -1) i32 @int_k1_i32(i32 %0) local_unnamed_addr #0 {
  %2 = shl i32 %0, 1
  ret i32 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @int_k2_i32(i32 %0) local_unnamed_addr #0 {
  %2 = mul i32 %0, %0
  ret i32 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @int_k3_i32(i32 %0) local_unnamed_addr #0 {
  %2 = mul i32 %0, 3
  ret i32 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @int_k4_i32(i32 %0) local_unnamed_addr #0 {
  %2 = add i32 %0, 1
  %3 = mul i32 %2, %0
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @int_k5_i32(i32 %0) local_unnamed_addr #0 {
  %2 = shl i32 %0, 1
  %3 = mul i32 %2, %2
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @int_k6_i32(i32 %0) local_unnamed_addr #0 {
  %2 = mul i32 %0, %0
  %3 = mul i32 %2, %0
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i32 0, -1) i32 @int_k7_i32(i32 %0) local_unnamed_addr #0 {
  %2 = shl i32 %0, 1
  %3 = mul i32 %2, %0
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @int_k8_i32(i32 %0) local_unnamed_addr #0 {
  %2 = mul i32 %0, 3
  %3 = mul i32 %2, %0
  ret i32 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i64 0, -1) i64 @int_k1_i64(i64 %0) local_unnamed_addr #0 {
  %2 = shl i64 %0, 1
  ret i64 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @int_k2_i64(i64 %0) local_unnamed_addr #0 {
  %2 = mul i64 %0, %0
  ret i64 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @int_k3_i64(i64 %0) local_unnamed_addr #0 {
  %2 = mul i64 %0, 3
  ret i64 %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @int_k4_i64(i64 %0) local_unnamed_addr #0 {
  %2 = add i64 %0, 1
  %3 = mul i64 %2, %0
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @int_k5_i64(i64 %0) local_unnamed_addr #0 {
  %2 = shl i64 %0, 1
  %3 = mul i64 %2, %2
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @int_k6_i64(i64 %0) local_unnamed_addr #0 {
  %2 = mul i64 %0, %0
  %3 = mul i64 %2, %0
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define range(i64 0, -1) i64 @int_k7_i64(i64 %0) local_unnamed_addr #0 {
  %2 = shl i64 %0, 1
  %3 = mul i64 %2, %0
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @int_k8_i64(i64 %0) local_unnamed_addr #0 {
  %2 = mul i64 %0, 3
  %3 = mul i64 %2, %0
  ret i64 %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @float_k1_f32(float %0) local_unnamed_addr #0 {
  %2 = fadd float %0, %0
  ret float %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @float_k2_f32(float %0) local_unnamed_addr #0 {
  %2 = fmul float %0, %0
  ret float %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @float_k3_f32(float %0) local_unnamed_addr #0 {
  %2 = fadd float %0, %0
  %3 = fadd float %0, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @float_k4_f32(float %0) local_unnamed_addr #0 {
  %2 = fmul float %0, %0
  %3 = fadd float %0, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @float_k5_f32(float %0) local_unnamed_addr #0 {
  %2 = fadd float %0, %0
  %3 = fmul float %2, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @float_k6_f32(float %0) local_unnamed_addr #0 {
  %2 = fmul float %0, %0
  %3 = fmul float %0, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @float_k7_f32(float %0) local_unnamed_addr #0 {
  %2 = fmul float %0, %0
  %3 = fadd float %2, %2
  ret float %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @float_k8_f32(float %0) local_unnamed_addr #0 {
  %2 = fadd float %0, %0
  %3 = fadd float %0, %2
  %4 = fmul float %0, %3
  ret float %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @float_k1_f64(double %0) local_unnamed_addr #0 {
  %2 = fadd double %0, %0
  ret double %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @float_k2_f64(double %0) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  ret double %2
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @float_k3_f64(double %0) local_unnamed_addr #0 {
  %2 = fadd double %0, %0
  %3 = fadd double %0, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @float_k4_f64(double %0) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  %3 = fadd double %0, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @float_k5_f64(double %0) local_unnamed_addr #0 {
  %2 = fadd double %0, %0
  %3 = fmul double %2, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @float_k6_f64(double %0) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  %3 = fmul double %0, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @float_k7_f64(double %0) local_unnamed_addr #0 {
  %2 = fmul double %0, %0
  %3 = fadd double %2, %2
  ret double %3
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @float_k8_f64(double %0) local_unnamed_addr #0 {
  %2 = fadd double %0, %0
  %3 = fadd double %0, %2
  %4 = fmul double %0, %3
  ret double %4
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @shared_polymorphic_kernel_bank_multitype(i8 %0, i16 %1, i32 %2, i64 %3, float %4, double %5) local_unnamed_addr #0 {
  %7 = shl i8 %0, 1
  %8 = mul i8 %0, %0
  %9 = mul i8 %0, 3
  %10 = add i8 %0, 1
  %11 = mul i8 %10, %0
  %12 = mul i8 %7, %7
  %13 = mul i8 %8, %0
  %14 = mul i8 %7, %0
  %15 = mul i8 %9, %0
  %16 = shl i16 %1, 1
  %17 = mul i16 %1, %1
  %18 = mul i16 %1, 3
  %19 = add i16 %1, 1
  %20 = mul i16 %19, %1
  %21 = mul i16 %16, %16
  %22 = mul i16 %17, %1
  %23 = mul i16 %16, %1
  %24 = mul i16 %18, %1
  %25 = shl i32 %2, 1
  %26 = mul i32 %2, %2
  %27 = mul i32 %2, 3
  %28 = add i32 %2, 1
  %29 = mul i32 %28, %2
  %30 = mul i32 %25, %25
  %31 = mul i32 %26, %2
  %32 = mul i32 %25, %2
  %33 = mul i32 %27, %2
  %34 = shl i64 %3, 1
  %35 = mul i64 %3, %3
  %36 = mul i64 %3, 3
  %37 = mul i64 %34, %34
  %38 = fadd float %4, %4
  %39 = fmul float %4, %4
  %40 = fadd float %4, %38
  %41 = fadd float %4, %39
  %42 = fmul float %38, %38
  %43 = fmul float %4, %39
  %44 = fadd float %39, %39
  %45 = fmul float %4, %40
  %46 = fadd double %5, %5
  %47 = fmul double %5, %5
  %48 = fadd double %5, %46
  %49 = fadd double %5, %47
  %50 = fmul double %46, %46
  %51 = fmul double %5, %47
  %52 = fadd double %47, %47
  %53 = fmul double %5, %48
  %54 = sext i8 %7 to i64
  %55 = sext i8 %8 to i64
  %56 = sext i8 %9 to i64
  %57 = sext i8 %11 to i64
  %58 = sext i8 %12 to i64
  %59 = sext i8 %13 to i64
  %60 = sext i8 %14 to i64
  %61 = sext i8 %15 to i64
  %62 = sext i16 %16 to i64
  %63 = sext i16 %17 to i64
  %64 = sext i16 %18 to i64
  %65 = sext i16 %20 to i64
  %66 = sext i16 %21 to i64
  %67 = sext i16 %22 to i64
  %68 = sext i16 %23 to i64
  %69 = sext i16 %24 to i64
  %70 = sext i32 %25 to i64
  %71 = sext i32 %26 to i64
  %72 = sext i32 %27 to i64
  %73 = sext i32 %29 to i64
  %74 = sext i32 %30 to i64
  %75 = sext i32 %31 to i64
  %76 = sext i32 %32 to i64
  %77 = sext i32 %33 to i64
  %78 = fptosi float %38 to i64
  %79 = fptosi float %39 to i64
  %80 = fptosi float %40 to i64
  %81 = fptosi float %41 to i64
  %82 = fptosi float %42 to i64
  %83 = fptosi float %43 to i64
  %84 = fptosi float %44 to i64
  %85 = fptosi float %45 to i64
  %86 = fptosi double %46 to i64
  %87 = fptosi double %47 to i64
  %88 = fptosi double %48 to i64
  %89 = fptosi double %49 to i64
  %90 = fptosi double %50 to i64
  %91 = fptosi double %51 to i64
  %92 = fptosi double %52 to i64
  %93 = fptosi double %53 to i64
  %reass.add = add i64 %3, 1
  %reass.add1 = add i64 %reass.add, %34
  %94 = add i64 %reass.add1, %36
  %reass.add2 = add i64 %94, %35
  %reass.mul = mul i64 %reass.add2, %3
  %95 = add nsw i64 %54, %55
  %96 = add nsw i64 %95, %56
  %97 = add nsw i64 %96, %57
  %98 = add nsw i64 %97, %58
  %99 = add nsw i64 %98, %59
  %100 = add nsw i64 %99, %60
  %101 = add nsw i64 %100, %61
  %102 = add nsw i64 %101, %62
  %103 = add nsw i64 %102, %63
  %104 = add nsw i64 %103, %64
  %105 = add nsw i64 %104, %65
  %106 = add nsw i64 %105, %66
  %107 = add nsw i64 %106, %67
  %108 = add nsw i64 %107, %68
  %109 = add nsw i64 %108, %69
  %110 = add nsw i64 %109, %70
  %111 = add nsw i64 %110, %71
  %112 = add nsw i64 %111, %72
  %113 = add i64 %112, %34
  %114 = add i64 %113, %35
  %115 = add i64 %114, %36
  %116 = add i64 %115, %73
  %117 = add i64 %116, %74
  %118 = add i64 %117, %75
  %119 = add i64 %118, %76
  %120 = add i64 %119, %77
  %121 = add i64 %120, %37
  %122 = add i64 %121, %78
  %123 = add i64 %122, %79
  %124 = add i64 %123, %80
  %125 = add i64 %124, %81
  %126 = add i64 %125, %82
  %127 = add i64 %126, %83
  %128 = add i64 %127, %84
  %129 = add i64 %128, %86
  %130 = add i64 %129, %87
  %131 = add i64 %130, %85
  %132 = add i64 %131, %88
  %133 = add i64 %132, %89
  %134 = add i64 %133, %90
  %135 = add i64 %134, %91
  %136 = add i64 %135, %92
  %137 = add i64 %136, %reass.mul
  %138 = add i64 %137, %93
  ret i64 %138
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

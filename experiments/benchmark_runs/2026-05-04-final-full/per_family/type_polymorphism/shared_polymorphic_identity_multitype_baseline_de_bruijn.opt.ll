; ModuleID = '/home/dominic/dev/scair/experiments/type_polymorphism/out/shared_polymorphic_identity_multitype_baseline_de_bruijn.ll'
source_filename = "LLVMDialectModule"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define double @lifted_6(double returned %0) local_unnamed_addr #0 {
  ret double %0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define float @lifted_5(float returned %0) local_unnamed_addr #0 {
  ret float %0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @lifted_4(i64 returned %0) local_unnamed_addr #0 {
  ret i64 %0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i32 @lifted_3(i32 returned %0) local_unnamed_addr #0 {
  ret i32 %0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i16 @lifted_2(i16 returned %0) local_unnamed_addr #0 {
  ret i16 %0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i8 @lifted_1(i8 returned %0) local_unnamed_addr #0 {
  ret i8 %0
}

; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define i64 @shared_polymorphic_identity_multitype(i8 %0, i16 %1, i32 %2, i64 %3, float %4, double %5) local_unnamed_addr #0 {
  %7 = sext i8 %0 to i64
  %8 = sext i16 %1 to i64
  %9 = sext i32 %2 to i64
  %10 = fptosi float %4 to i64
  %11 = fptosi double %5 to i64
  %12 = add nsw i64 %8, %7
  %13 = add nsw i64 %12, %9
  %14 = add i64 %13, %3
  %15 = add i64 %14, %10
  %16 = add i64 %15, %11
  ret i64 %16
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

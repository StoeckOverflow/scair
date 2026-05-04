; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define double @lifted_6(double %0) {
  ret double %0
}

define float @lifted_5(float %0) {
  ret float %0
}

define i64 @lifted_4(i64 %0) {
  ret i64 %0
}

define i32 @lifted_3(i32 %0) {
  ret i32 %0
}

define i16 @lifted_2(i16 %0) {
  ret i16 %0
}

define i8 @lifted_1(i8 %0) {
  ret i8 %0
}

define i64 @shared_polymorphic_identity_multitype(i8 %0, i16 %1, i32 %2, i64 %3, float %4, double %5) {
  %7 = sext i8 %0 to i64
  %8 = sext i16 %1 to i64
  %9 = sext i32 %2 to i64
  %10 = fptosi float %4 to i64
  %11 = fptosi double %5 to i64
  %12 = add i64 %7, %8
  %13 = add i64 %9, %3
  %14 = add i64 %10, %11
  %15 = add i64 %12, %13
  %16 = add i64 %15, %14
  ret i64 %16
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

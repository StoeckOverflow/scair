; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define i8 @sink_i8(i8 %0) {
  ret i8 %0
}

define i16 @sink_i16(i16 %0) {
  ret i16 %0
}

define i32 @sink_i32(i32 %0) {
  ret i32 %0
}

define i64 @sink_i64(i64 %0) {
  ret i64 %0
}

define float @sink_f32(float %0) {
  ret float %0
}

define double @sink_f64(double %0) {
  ret double %0
}

define i64 @shared_polymorphic_identity_multitype(i8 %0, i16 %1, i32 %2, i64 %3, float %4, double %5) {
  %7 = call i8 @sink_i8(i8 %0)
  %8 = call i16 @sink_i16(i16 %1)
  %9 = call i32 @sink_i32(i32 %2)
  %10 = call i64 @sink_i64(i64 %3)
  %11 = call float @sink_f32(float %4)
  %12 = call double @sink_f64(double %5)
  %13 = sext i8 %7 to i64
  %14 = sext i16 %8 to i64
  %15 = sext i32 %9 to i64
  %16 = fptosi float %11 to i64
  %17 = fptosi double %12 to i64
  %18 = add i64 %13, %14
  %19 = add i64 %15, %10
  %20 = add i64 %16, %17
  %21 = add i64 %18, %19
  %22 = add i64 %21, %20
  ret i64 %22
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

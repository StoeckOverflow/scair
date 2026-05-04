; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @broadcast_affine_2d(i64 %0, i64 %1, ptr %2, ptr %3, ptr %4, ptr %5) {
  %7 = mul i64 %0, %1
  br label %8

8:                                                ; preds = %29, %6
  %9 = phi i64 [ %30, %29 ], [ 0, %6 ]
  %10 = icmp slt i64 %9, %0
  br i1 %10, label %11, label %13

11:                                               ; preds = %8
  %12 = mul i64 %9, %1
  br label %14

13:                                               ; preds = %8
  ret void

14:                                               ; preds = %17, %11
  %15 = phi i64 [ %28, %17 ], [ 0, %11 ]
  %16 = icmp slt i64 %15, %1
  br i1 %16, label %17, label %29

17:                                               ; preds = %14
  %18 = add i64 %12, %15
  %19 = getelementptr i64, ptr %2, i64 %18
  %20 = load i64, ptr %19, align 4
  %21 = getelementptr i64, ptr %3, i64 %15
  %22 = load i64, ptr %21, align 4
  %23 = getelementptr i64, ptr %4, i64 %15
  %24 = load i64, ptr %23, align 4
  %25 = mul i64 %20, %22
  %26 = add i64 %25, %24
  %27 = getelementptr i64, ptr %5, i64 %18
  store i64 %26, ptr %27, align 4
  %28 = add i64 %15, 1
  br label %14

29:                                               ; preds = %14
  %30 = add i64 %9, 1
  br label %8
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

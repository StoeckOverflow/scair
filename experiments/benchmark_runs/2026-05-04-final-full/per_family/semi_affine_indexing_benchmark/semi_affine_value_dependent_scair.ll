; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @semi_affine_fill_and_sum(i64 %0, i64 %1, ptr %2, ptr %3) {
  %5 = add i64 %0, 0
  %6 = add i64 %1, 0
  br label %7

7:                                                ; preds = %21, %4
  %8 = phi i64 [ %22, %21 ], [ 0, %4 ]
  %9 = icmp slt i64 %8, 256
  br i1 %9, label %10, label %11

10:                                               ; preds = %7
  br label %12

11:                                               ; preds = %7
  br label %23

12:                                               ; preds = %15, %10
  %13 = phi i64 [ %20, %15 ], [ 0, %10 ]
  %14 = icmp slt i64 %13, 1024
  br i1 %14, label %15, label %21

15:                                               ; preds = %12
  %16 = mul i64 %8, %5
  %17 = mul i64 %13, %6
  %18 = add i64 %16, %17
  %19 = getelementptr float, ptr %2, i64 %18
  store float 1.000000e+00, ptr %19, align 4
  %20 = add i64 %13, 1
  br label %12

21:                                               ; preds = %12
  %22 = add i64 %8, 1
  br label %7

23:                                               ; preds = %41, %11
  %24 = phi i64 [ %42, %41 ], [ 0, %11 ]
  %25 = phi float [ %31, %41 ], [ 0.000000e+00, %11 ]
  %26 = icmp slt i64 %24, 256
  br i1 %26, label %27, label %43

27:                                               ; preds = %23
  br label %28

28:                                               ; preds = %33, %27
  %29 = phi i64 [ %29, %33 ], [ %24, %27 ]
  %30 = phi i64 [ %40, %33 ], [ 0, %27 ]
  %31 = phi float [ %39, %33 ], [ %25, %27 ]
  %32 = icmp slt i64 %30, 1024
  br i1 %32, label %33, label %41

33:                                               ; preds = %28
  %34 = mul i64 %29, %5
  %35 = mul i64 %30, %6
  %36 = add i64 %34, %35
  %37 = getelementptr float, ptr %2, i64 %36
  %38 = load float, ptr %37, align 4
  %39 = fadd float %31, %38
  %40 = add i64 %30, 1
  br label %28

41:                                               ; preds = %28
  %42 = add i64 %29, 1
  br label %23

43:                                               ; preds = %23
  %44 = getelementptr float, ptr %3, i64 0
  store float %25, ptr %44, align 4
  ret void
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

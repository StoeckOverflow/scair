; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @matmul_tiling(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5, ptr %6) {
  %8 = mul i64 %2, %3
  br label %9

9:                                                ; preds = %18, %7
  %10 = phi i64 [ %19, %18 ], [ 0, %7 ]
  %11 = icmp slt i64 %10, %0
  br i1 %11, label %12, label %13

12:                                               ; preds = %9
  br label %14

13:                                               ; preds = %9
  ret void

14:                                               ; preds = %46, %12
  %15 = phi i64 [ %50, %46 ], [ 0, %12 ]
  %16 = icmp slt i64 %15, %1
  br i1 %16, label %17, label %18

17:                                               ; preds = %14
  br label %20

18:                                               ; preds = %14
  %19 = add i64 %10, 1
  br label %9

20:                                               ; preds = %44, %17
  %21 = phi i64 [ %45, %44 ], [ 0, %17 ]
  %22 = phi float [ %28, %44 ], [ 0.000000e+00, %17 ]
  %23 = icmp slt i64 %21, %2
  br i1 %23, label %24, label %46

24:                                               ; preds = %20
  br label %25

25:                                               ; preds = %30, %24
  %26 = phi i64 [ %26, %30 ], [ %21, %24 ]
  %27 = phi i64 [ %43, %30 ], [ 0, %24 ]
  %28 = phi float [ %42, %30 ], [ %22, %24 ]
  %29 = icmp slt i64 %27, %3
  br i1 %29, label %30, label %44

30:                                               ; preds = %25
  %31 = mul i64 %26, %3
  %32 = add i64 %31, %27
  %33 = mul i64 %10, %8
  %34 = add i64 %33, %32
  %35 = getelementptr float, ptr %4, i64 %34
  %36 = load float, ptr %35, align 4
  %37 = mul i64 %32, %1
  %38 = add i64 %37, %15
  %39 = getelementptr float, ptr %5, i64 %38
  %40 = load float, ptr %39, align 4
  %41 = fmul float %36, %40
  %42 = fadd float %28, %41
  %43 = add i64 %27, 1
  br label %25

44:                                               ; preds = %25
  %45 = add i64 %26, 1
  br label %20

46:                                               ; preds = %20
  %47 = mul i64 %10, %1
  %48 = add i64 %47, %15
  %49 = getelementptr float, ptr %6, i64 %48
  store float %22, ptr %49, align 4
  %50 = add i64 %15, 1
  br label %14
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

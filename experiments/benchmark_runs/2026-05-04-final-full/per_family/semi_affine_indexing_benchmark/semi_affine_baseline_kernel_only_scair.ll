; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @semi_affine_fill_and_sum(i64 %0, i64 %1, ptr %2, ptr %3) {
  %5 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %2, align 8
  %6 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %3, align 8
  %7 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %5, 0
  %8 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %5, 1
  %9 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %7, 0
  %10 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %9, ptr %8, 1
  %11 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %10, i64 0, 2
  %12 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %11, i64 256, 3, 0
  %13 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %12, i64 1024, 3, 1
  %14 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %13, i64 %0, 4, 0
  %15 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %14, i64 %1, 4, 1
  br label %16

16:                                               ; preds = %33, %4
  %17 = phi i64 [ %34, %33 ], [ 0, %4 ]
  %18 = icmp slt i64 %17, 256
  br i1 %18, label %19, label %20

19:                                               ; preds = %16
  br label %21

20:                                               ; preds = %16
  br label %35

21:                                               ; preds = %24, %19
  %22 = phi i64 [ %32, %24 ], [ 0, %19 ]
  %23 = icmp slt i64 %22, 1024
  br i1 %23, label %24, label %33

24:                                               ; preds = %21
  %25 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %15, 1
  %26 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %15, 4, 0
  %27 = mul i64 %17, %26
  %28 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %15, 4, 1
  %29 = mul i64 %22, %28
  %30 = add i64 %27, %29
  %31 = getelementptr float, ptr %25, i64 %30
  store float 1.000000e+00, ptr %31, align 4
  %32 = add i64 %22, 1
  br label %21

33:                                               ; preds = %21
  %34 = add i64 %17, 1
  br label %16

35:                                               ; preds = %55, %20
  %36 = phi i64 [ %56, %55 ], [ 0, %20 ]
  %37 = phi float [ %42, %55 ], [ 0.000000e+00, %20 ]
  %38 = icmp slt i64 %36, 256
  br i1 %38, label %39, label %57

39:                                               ; preds = %35
  br label %40

40:                                               ; preds = %44, %39
  %41 = phi i64 [ %54, %44 ], [ 0, %39 ]
  %42 = phi float [ %53, %44 ], [ %37, %39 ]
  %43 = icmp slt i64 %41, 1024
  br i1 %43, label %44, label %55

44:                                               ; preds = %40
  %45 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %15, 1
  %46 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %15, 4, 0
  %47 = mul i64 %36, %46
  %48 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %15, 4, 1
  %49 = mul i64 %41, %48
  %50 = add i64 %47, %49
  %51 = getelementptr float, ptr %45, i64 %50
  %52 = load float, ptr %51, align 4
  %53 = fadd float %42, %52
  %54 = add i64 %41, 1
  br label %40

55:                                               ; preds = %40
  %56 = add i64 %36, 1
  br label %35

57:                                               ; preds = %35
  %58 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %6, 1
  %59 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %6, 4, 0
  %60 = mul i64 0, %59
  %61 = getelementptr float, ptr %58, i64 %60
  store float %37, ptr %61, align 4
  ret void
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

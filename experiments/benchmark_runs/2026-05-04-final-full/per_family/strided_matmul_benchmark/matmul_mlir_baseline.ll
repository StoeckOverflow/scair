; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @matmul_strided(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, i64 %11, i64 %12, i64 %13, ptr %14, ptr %15, i64 %16, i64 %17, i64 %18, ptr %19, ptr %20, i64 %21, i64 %22, i64 %23) {
  %25 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %19, 0
  %26 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, ptr %20, 1
  %27 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %26, i64 %21, 2
  %28 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %27, i64 %22, 3, 0
  %29 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %28, i64 %23, 4, 0
  %30 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %14, 0
  %31 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, ptr %15, 1
  %32 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %31, i64 %16, 2
  %33 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %32, i64 %17, 3, 0
  %34 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %33, i64 %18, 4, 0
  %35 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %9, 0
  %36 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %35, ptr %10, 1
  %37 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %36, i64 %11, 2
  %38 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %37, i64 %12, 3, 0
  %39 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %38, i64 %13, 4, 0
  %40 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %39, 0
  %41 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %39, 1
  %42 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %40, 0
  %43 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %42, ptr %41, 1
  %44 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %43, i64 0, 2
  %45 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %44, i64 %0, 3, 0
  %46 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %45, i64 %3, 4, 0
  %47 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %46, i64 %2, 3, 1
  %48 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %47, i64 %4, 4, 1
  %49 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %34, 0
  %50 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %34, 1
  %51 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %49, 0
  %52 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %51, ptr %50, 1
  %53 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %52, i64 0, 2
  %54 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %53, i64 %2, 3, 0
  %55 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %54, i64 %5, 4, 0
  %56 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %55, i64 %1, 3, 1
  %57 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %56, i64 %6, 4, 1
  %58 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %29, 0
  %59 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %29, 1
  %60 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %58, 0
  %61 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %60, ptr %59, 1
  %62 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %61, i64 0, 2
  %63 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %62, i64 %0, 3, 0
  %64 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %63, i64 %7, 4, 0
  %65 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %64, i64 %1, 3, 1
  %66 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %65, i64 %8, 4, 1
  br label %67

67:                                               ; preds = %108, %24
  %68 = phi i64 [ %109, %108 ], [ 0, %24 ]
  %69 = icmp slt i64 %68, %0
  br i1 %69, label %70, label %110

70:                                               ; preds = %67
  br label %71

71:                                               ; preds = %99, %70
  %72 = phi i64 [ %107, %99 ], [ 0, %70 ]
  %73 = icmp slt i64 %72, %1
  br i1 %73, label %74, label %108

74:                                               ; preds = %71
  br label %75

75:                                               ; preds = %79, %74
  %76 = phi i64 [ %98, %79 ], [ 0, %74 ]
  %77 = phi float [ %97, %79 ], [ 0.000000e+00, %74 ]
  %78 = icmp slt i64 %76, %2
  br i1 %78, label %79, label %99

79:                                               ; preds = %75
  %80 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %48, 1
  %81 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %48, 4, 0
  %82 = mul nuw nsw i64 %68, %81
  %83 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %48, 4, 1
  %84 = mul nuw nsw i64 %76, %83
  %85 = add nuw nsw i64 %82, %84
  %86 = getelementptr inbounds nuw float, ptr %80, i64 %85
  %87 = load float, ptr %86, align 4
  %88 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %57, 1
  %89 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %57, 4, 0
  %90 = mul nuw nsw i64 %76, %89
  %91 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %57, 4, 1
  %92 = mul nuw nsw i64 %72, %91
  %93 = add nuw nsw i64 %90, %92
  %94 = getelementptr inbounds nuw float, ptr %88, i64 %93
  %95 = load float, ptr %94, align 4
  %96 = fmul float %87, %95
  %97 = fadd float %77, %96
  %98 = add i64 %76, 1
  br label %75

99:                                               ; preds = %75
  %100 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %66, 1
  %101 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %66, 4, 0
  %102 = mul nuw nsw i64 %68, %101
  %103 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %66, 4, 1
  %104 = mul nuw nsw i64 %72, %103
  %105 = add nuw nsw i64 %102, %104
  %106 = getelementptr inbounds nuw float, ptr %100, i64 %105
  store float %77, ptr %106, align 4
  %107 = add i64 %72, 1
  br label %71

108:                                              ; preds = %71
  %109 = add i64 %68, 1
  br label %67

110:                                              ; preds = %67
  ret void
}

define void @_mlir_ciface_matmul_strided(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, ptr %11) {
  %13 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %9, align 8
  %14 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 0
  %15 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 1
  %16 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 2
  %17 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 3, 0
  %18 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 4, 0
  %19 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %10, align 8
  %20 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 0
  %21 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 1
  %22 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 2
  %23 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 3, 0
  %24 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 4, 0
  %25 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %11, align 8
  %26 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 0
  %27 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 1
  %28 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 2
  %29 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 3, 0
  %30 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 4, 0
  call void @matmul_strided(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %14, ptr %15, i64 %16, i64 %17, i64 %18, ptr %20, ptr %21, i64 %22, i64 %23, i64 %24, ptr %26, ptr %27, i64 %28, i64 %29, i64 %30)
  ret void
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

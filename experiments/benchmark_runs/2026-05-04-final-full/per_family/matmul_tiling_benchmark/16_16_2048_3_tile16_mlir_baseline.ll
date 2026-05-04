; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @matmul_tiling(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, i64 %11, i64 %12, i64 %13, ptr %14, ptr %15, i64 %16, i64 %17, i64 %18) {
  %20 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %14, 0
  %21 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %20, ptr %15, 1
  %22 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %21, i64 %16, 2
  %23 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %22, i64 %17, 3, 0
  %24 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %23, i64 %18, 4, 0
  %25 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %9, 0
  %26 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, ptr %10, 1
  %27 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %26, i64 %11, 2
  %28 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %27, i64 %12, 3, 0
  %29 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %28, i64 %13, 4, 0
  %30 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %4, 0
  %31 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, ptr %5, 1
  %32 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %31, i64 %6, 2
  %33 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %32, i64 %7, 3, 0
  %34 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %33, i64 %8, 4, 0
  %35 = mul i64 %2, %3
  %36 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %34, 0
  %37 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %34, 1
  %38 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %36, 0
  %39 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %38, ptr %37, 1
  %40 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %39, i64 0, 2
  %41 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %40, i64 %0, 3, 0
  %42 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %41, i64 %35, 4, 0
  %43 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %42, i64 %35, 3, 1
  %44 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %43, i64 1, 4, 1
  %45 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %29, 0
  %46 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %29, 1
  %47 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %45, 0
  %48 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %47, ptr %46, 1
  %49 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %48, i64 0, 2
  %50 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %49, i64 %35, 3, 0
  %51 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %50, i64 %1, 4, 0
  %52 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %51, i64 %1, 3, 1
  %53 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %52, i64 1, 4, 1
  %54 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, 0
  %55 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, 1
  %56 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %54, 0
  %57 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %56, ptr %55, 1
  %58 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %57, i64 0, 2
  %59 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %58, i64 %0, 3, 0
  %60 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %59, i64 %1, 4, 0
  %61 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %60, i64 %1, 3, 1
  %62 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %61, i64 1, 4, 1
  br label %63

63:                                               ; preds = %126, %19
  %64 = phi i64 [ %127, %126 ], [ 0, %19 ]
  %65 = icmp slt i64 %64, %0
  br i1 %65, label %66, label %128

66:                                               ; preds = %63
  br label %67

67:                                               ; preds = %124, %66
  %68 = phi i64 [ %125, %124 ], [ 0, %66 ]
  %69 = icmp slt i64 %68, %1
  br i1 %69, label %70, label %126

70:                                               ; preds = %67
  %71 = add i64 %64, 16
  %72 = call i64 @llvm.smin.i64(i64 %71, i64 %0)
  br label %73

73:                                               ; preds = %122, %70
  %74 = phi i64 [ %123, %122 ], [ %64, %70 ]
  %75 = icmp slt i64 %74, %72
  br i1 %75, label %76, label %124

76:                                               ; preds = %73
  %77 = add i64 %68, 16
  %78 = call i64 @llvm.smin.i64(i64 %77, i64 %1)
  br label %79

79:                                               ; preds = %111, %76
  %80 = phi i64 [ %121, %111 ], [ %68, %76 ]
  %81 = icmp slt i64 %80, %78
  br i1 %81, label %82, label %122

82:                                               ; preds = %79
  br label %83

83:                                               ; preds = %87, %82
  %84 = phi i64 [ %110, %87 ], [ 0, %82 ]
  %85 = phi float [ %109, %87 ], [ 0.000000e+00, %82 ]
  %86 = icmp slt i64 %84, %35
  br i1 %86, label %87, label %111

87:                                               ; preds = %83
  %88 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %44, 1
  %89 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %44, 2
  %90 = getelementptr float, ptr %88, i64 %89
  %91 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %44, 4, 0
  %92 = mul nuw nsw i64 %74, %91
  %93 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %44, 4, 1
  %94 = mul nuw nsw i64 %84, %93
  %95 = add nuw nsw i64 %92, %94
  %96 = getelementptr inbounds nuw float, ptr %90, i64 %95
  %97 = load float, ptr %96, align 4
  %98 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %53, 1
  %99 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %53, 2
  %100 = getelementptr float, ptr %98, i64 %99
  %101 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %53, 4, 0
  %102 = mul nuw nsw i64 %84, %101
  %103 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %53, 4, 1
  %104 = mul nuw nsw i64 %80, %103
  %105 = add nuw nsw i64 %102, %104
  %106 = getelementptr inbounds nuw float, ptr %100, i64 %105
  %107 = load float, ptr %106, align 4
  %108 = fmul float %97, %107
  %109 = fadd float %85, %108
  %110 = add i64 %84, 1
  br label %83

111:                                              ; preds = %83
  %112 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %62, 1
  %113 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %62, 2
  %114 = getelementptr float, ptr %112, i64 %113
  %115 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %62, 4, 0
  %116 = mul nuw nsw i64 %74, %115
  %117 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %62, 4, 1
  %118 = mul nuw nsw i64 %80, %117
  %119 = add nuw nsw i64 %116, %118
  %120 = getelementptr inbounds nuw float, ptr %114, i64 %119
  store float %85, ptr %120, align 4
  %121 = add i64 %80, 1
  br label %79

122:                                              ; preds = %79
  %123 = add i64 %74, 1
  br label %73

124:                                              ; preds = %73
  %125 = add i64 %68, 16
  br label %67

126:                                              ; preds = %67
  %127 = add i64 %64, 16
  br label %63

128:                                              ; preds = %63
  ret void
}

define void @_mlir_ciface_matmul_tiling(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5, ptr %6) {
  %8 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %4, align 8
  %9 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 0
  %10 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 1
  %11 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 2
  %12 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 3, 0
  %13 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 4, 0
  %14 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %5, align 8
  %15 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 0
  %16 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 1
  %17 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 2
  %18 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 3, 0
  %19 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 4, 0
  %20 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %6, align 8
  %21 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %20, 0
  %22 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %20, 1
  %23 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %20, 2
  %24 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %20, 3, 0
  %25 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %20, 4, 0
  call void @matmul_tiling(i64 %0, i64 %1, i64 %2, i64 %3, ptr %9, ptr %10, i64 %11, i64 %12, i64 %13, ptr %15, ptr %16, i64 %17, i64 %18, i64 %19, ptr %21, ptr %22, i64 %23, i64 %24, i64 %25)
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.smin.i64(i64, i64) #0

attributes #0 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

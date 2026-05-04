; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @matmul_strided(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, ptr %11) {
  %13 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %9, align 8
  %14 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %10, align 8
  %15 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %11, align 8
  %16 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 0
  %17 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 1
  %18 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %16, 0
  %19 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %18, ptr %17, 1
  %20 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %19, i64 0, 2
  %21 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %20, i64 %0, 3, 0
  %22 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %21, i64 %2, 3, 1
  %23 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %22, i64 %3, 4, 0
  %24 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %23, i64 %4, 4, 1
  %25 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 0
  %26 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 1
  %27 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %25, 0
  %28 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %27, ptr %26, 1
  %29 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %28, i64 0, 2
  %30 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %29, i64 %2, 3, 0
  %31 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %30, i64 %1, 3, 1
  %32 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %31, i64 %5, 4, 0
  %33 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %32, i64 %6, 4, 1
  %34 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %15, 0
  %35 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %15, 1
  %36 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %34, 0
  %37 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %36, ptr %35, 1
  %38 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %37, i64 0, 2
  %39 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %38, i64 %0, 3, 0
  %40 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %39, i64 %1, 3, 1
  %41 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %40, i64 %7, 4, 0
  %42 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %41, i64 %8, 4, 1
  br label %43

43:                                               ; preds = %52, %12
  %44 = phi i64 [ %53, %52 ], [ 0, %12 ]
  %45 = icmp slt i64 %44, %0
  br i1 %45, label %46, label %47

46:                                               ; preds = %43
  br label %48

47:                                               ; preds = %43
  ret void

48:                                               ; preds = %78, %46
  %49 = phi i64 [ %86, %78 ], [ 0, %46 ]
  %50 = icmp slt i64 %49, %1
  br i1 %50, label %51, label %52

51:                                               ; preds = %48
  br label %54

52:                                               ; preds = %48
  %53 = add i64 %44, 1
  br label %43

54:                                               ; preds = %58, %51
  %55 = phi i64 [ %77, %58 ], [ 0, %51 ]
  %56 = phi float [ %76, %58 ], [ 0.000000e+00, %51 ]
  %57 = icmp slt i64 %55, %2
  br i1 %57, label %58, label %78

58:                                               ; preds = %54
  %59 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %24, 1
  %60 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %24, 4, 0
  %61 = mul i64 %44, %60
  %62 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %24, 4, 1
  %63 = mul i64 %55, %62
  %64 = add i64 %61, %63
  %65 = getelementptr float, ptr %59, i64 %64
  %66 = load float, ptr %65, align 4
  %67 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %33, 1
  %68 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %33, 4, 0
  %69 = mul i64 %55, %68
  %70 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %33, 4, 1
  %71 = mul i64 %49, %70
  %72 = add i64 %69, %71
  %73 = getelementptr float, ptr %67, i64 %72
  %74 = load float, ptr %73, align 4
  %75 = fmul float %66, %74
  %76 = fadd float %56, %75
  %77 = add i64 %55, 1
  br label %54

78:                                               ; preds = %54
  %79 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %42, 1
  %80 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %42, 4, 0
  %81 = mul i64 %44, %80
  %82 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %42, 4, 1
  %83 = mul i64 %49, %82
  %84 = add i64 %81, %83
  %85 = getelementptr float, ptr %79, i64 %84
  store float %56, ptr %85, align 4
  %86 = add i64 %49, 1
  br label %48
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

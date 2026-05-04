; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @matmul_tiling(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5, ptr %6) {
  %8 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %4, align 8
  %9 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %5, align 8
  %10 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %6, align 8
  %11 = mul i64 %2, %3
  %12 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 0
  %13 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 1
  %14 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %12, 0
  %15 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %14, ptr %13, 1
  %16 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %15, i64 0, 2
  %17 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %16, i64 %0, 3, 0
  %18 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %17, i64 %11, 3, 1
  %19 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %18, i64 %11, 4, 0
  %20 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %19, i64 1, 4, 1
  %21 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %9, 0
  %22 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %9, 1
  %23 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %21, 0
  %24 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %23, ptr %22, 1
  %25 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %24, i64 0, 2
  %26 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %25, i64 %11, 3, 0
  %27 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %26, i64 %1, 3, 1
  %28 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %27, i64 %1, 4, 0
  %29 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %28, i64 1, 4, 1
  %30 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %10, 0
  %31 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %10, 1
  %32 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %30, 0
  %33 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %32, ptr %31, 1
  %34 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %33, i64 0, 2
  %35 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %34, i64 %0, 3, 0
  %36 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %35, i64 %1, 3, 1
  %37 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %36, i64 %1, 4, 0
  %38 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %37, i64 1, 4, 1
  br label %39

39:                                               ; preds = %48, %7
  %40 = phi i64 [ %49, %48 ], [ 0, %7 ]
  %41 = icmp slt i64 %40, %0
  br i1 %41, label %42, label %43

42:                                               ; preds = %39
  br label %44

43:                                               ; preds = %39
  ret void

44:                                               ; preds = %74, %42
  %45 = phi i64 [ %82, %74 ], [ 0, %42 ]
  %46 = icmp slt i64 %45, %1
  br i1 %46, label %47, label %48

47:                                               ; preds = %44
  br label %50

48:                                               ; preds = %44
  %49 = add i64 %40, 1
  br label %39

50:                                               ; preds = %54, %47
  %51 = phi i64 [ %73, %54 ], [ 0, %47 ]
  %52 = phi float [ %72, %54 ], [ 0.000000e+00, %47 ]
  %53 = icmp slt i64 %51, %11
  br i1 %53, label %54, label %74

54:                                               ; preds = %50
  %55 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %20, 1
  %56 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %20, 4, 0
  %57 = mul i64 %40, %56
  %58 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %20, 4, 1
  %59 = mul i64 %51, %58
  %60 = add i64 %57, %59
  %61 = getelementptr float, ptr %55, i64 %60
  %62 = load float, ptr %61, align 4
  %63 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %29, 1
  %64 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %29, 4, 0
  %65 = mul i64 %51, %64
  %66 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %29, 4, 1
  %67 = mul i64 %45, %66
  %68 = add i64 %65, %67
  %69 = getelementptr float, ptr %63, i64 %68
  %70 = load float, ptr %69, align 4
  %71 = fmul float %62, %70
  %72 = fadd float %52, %71
  %73 = add i64 %51, 1
  br label %50

74:                                               ; preds = %50
  %75 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %38, 1
  %76 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %38, 4, 0
  %77 = mul i64 %40, %76
  %78 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %38, 4, 1
  %79 = mul i64 %45, %78
  %80 = add i64 %77, %79
  %81 = getelementptr float, ptr %75, i64 %80
  store float %52, ptr %81, align 4
  %82 = add i64 %45, 1
  br label %44
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

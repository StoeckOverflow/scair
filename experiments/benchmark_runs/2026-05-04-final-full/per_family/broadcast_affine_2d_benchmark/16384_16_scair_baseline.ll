; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @broadcast_affine_2d(i64 %0, i64 %1, ptr %2, ptr %3, ptr %4, ptr %5) {
  %7 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %2, align 8
  %8 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %3, align 8
  %9 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %4, align 8
  %10 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %5, align 8
  %11 = mul i64 %0, %1
  %12 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 0
  %13 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 1
  %14 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %12, 0
  %15 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %14, ptr %13, 1
  %16 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %15, i64 0, 2
  %17 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %16, i64 %0, 3, 0
  %18 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %17, i64 %1, 3, 1
  %19 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %18, i64 %1, 4, 0
  %20 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %19, i64 1, 4, 1
  %21 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 0
  %22 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 1
  %23 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %21, 0
  %24 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %23, ptr %22, 1
  %25 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, i64 0, 2
  %26 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, i64 %1, 3, 0
  %27 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %26, i64 1, 4, 0
  %28 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %9, 0
  %29 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %9, 1
  %30 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %28, 0
  %31 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, ptr %29, 1
  %32 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %31, i64 0, 2
  %33 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %32, i64 %1, 3, 0
  %34 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %33, i64 1, 4, 0
  %35 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %10, 0
  %36 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %10, 1
  %37 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %35, 0
  %38 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %37, ptr %36, 1
  %39 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %38, i64 0, 2
  %40 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %39, i64 %0, 3, 0
  %41 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %40, i64 %1, 3, 1
  %42 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %41, i64 %1, 4, 0
  %43 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %42, i64 1, 4, 1
  br label %44

44:                                               ; preds = %81, %6
  %45 = phi i64 [ %82, %81 ], [ 0, %6 ]
  %46 = icmp slt i64 %45, %0
  br i1 %46, label %47, label %48

47:                                               ; preds = %44
  br label %49

48:                                               ; preds = %44
  ret void

49:                                               ; preds = %52, %47
  %50 = phi i64 [ %80, %52 ], [ 0, %47 ]
  %51 = icmp slt i64 %50, %1
  br i1 %51, label %52, label %81

52:                                               ; preds = %49
  %53 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %20, 1
  %54 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %20, 4, 0
  %55 = mul i64 %45, %54
  %56 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %20, 4, 1
  %57 = mul i64 %50, %56
  %58 = add i64 %55, %57
  %59 = getelementptr i64, ptr %53, i64 %58
  %60 = load i64, ptr %59, align 4
  %61 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %27, 1
  %62 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %27, 4, 0
  %63 = mul i64 %50, %62
  %64 = getelementptr i64, ptr %61, i64 %63
  %65 = load i64, ptr %64, align 4
  %66 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %34, 1
  %67 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %34, 4, 0
  %68 = mul i64 %50, %67
  %69 = getelementptr i64, ptr %66, i64 %68
  %70 = load i64, ptr %69, align 4
  %71 = mul i64 %60, %65
  %72 = add i64 %71, %70
  %73 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %43, 1
  %74 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %43, 4, 0
  %75 = mul i64 %45, %74
  %76 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %43, 4, 1
  %77 = mul i64 %50, %76
  %78 = add i64 %75, %77
  %79 = getelementptr i64, ptr %73, i64 %78
  store i64 %72, ptr %79, align 4
  %80 = add i64 %50, 1
  br label %49

81:                                               ; preds = %49
  %82 = add i64 %45, 1
  br label %44
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

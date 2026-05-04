; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @blocked_pack(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5) {
  %7 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %4, align 8
  %8 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %5, align 8
  %9 = mul i64 %0, %2
  %10 = mul i64 %1, %3
  %11 = mul i64 %2, %10
  %12 = mul i64 %3, 1
  %13 = mul i64 %2, %3
  %14 = mul i64 %1, %13
  %15 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 0
  %16 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 1
  %17 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %15, 0
  %18 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %17, ptr %16, 1
  %19 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %18, i64 0, 2
  %20 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %19, i64 %0, 3, 0
  %21 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %20, i64 %1, 3, 1
  %22 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %21, i64 %2, 3, 2
  %23 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %22, i64 %3, 3, 3
  %24 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %23, i64 %11, 4, 0
  %25 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %24, i64 %3, 4, 1
  %26 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %25, i64 %10, 4, 2
  %27 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %26, i64 1, 4, 3
  %28 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 0
  %29 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %8, 1
  %30 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %28, 0
  %31 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %30, ptr %29, 1
  %32 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %31, i64 0, 2
  %33 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %32, i64 %0, 3, 0
  %34 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %33, i64 %1, 3, 1
  %35 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %34, i64 %2, 3, 2
  %36 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %35, i64 %3, 3, 3
  %37 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %36, i64 %14, 4, 0
  %38 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %37, i64 %13, 4, 1
  %39 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %38, i64 %12, 4, 2
  %40 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %39, i64 1, 4, 3
  br label %41

41:                                               ; preds = %50, %6
  %42 = phi i64 [ %51, %50 ], [ 0, %6 ]
  %43 = icmp slt i64 %42, %0
  br i1 %43, label %44, label %45

44:                                               ; preds = %41
  br label %46

45:                                               ; preds = %41
  ret void

46:                                               ; preds = %56, %44
  %47 = phi i64 [ %57, %56 ], [ 0, %44 ]
  %48 = icmp slt i64 %47, %1
  br i1 %48, label %49, label %50

49:                                               ; preds = %46
  br label %52

50:                                               ; preds = %46
  %51 = add i64 %42, 1
  br label %41

52:                                               ; preds = %90, %49
  %53 = phi i64 [ %91, %90 ], [ 0, %49 ]
  %54 = icmp slt i64 %53, %2
  br i1 %54, label %55, label %56

55:                                               ; preds = %52
  br label %58

56:                                               ; preds = %52
  %57 = add i64 %47, 1
  br label %46

58:                                               ; preds = %61, %55
  %59 = phi i64 [ %89, %61 ], [ 0, %55 ]
  %60 = icmp slt i64 %59, %3
  br i1 %60, label %61, label %90

61:                                               ; preds = %58
  %62 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %27, 1
  %63 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %27, 4, 0
  %64 = mul i64 %42, %63
  %65 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %27, 4, 1
  %66 = mul i64 %47, %65
  %67 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %27, 4, 2
  %68 = mul i64 %53, %67
  %69 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %27, 4, 3
  %70 = mul i64 %59, %69
  %71 = add i64 %64, %66
  %72 = add i64 %71, %68
  %73 = add i64 %72, %70
  %74 = getelementptr i64, ptr %62, i64 %73
  %75 = load i64, ptr %74, align 4
  %76 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %40, 1
  %77 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %40, 4, 0
  %78 = mul i64 %42, %77
  %79 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %40, 4, 1
  %80 = mul i64 %47, %79
  %81 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %40, 4, 2
  %82 = mul i64 %53, %81
  %83 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %40, 4, 3
  %84 = mul i64 %59, %83
  %85 = add i64 %78, %80
  %86 = add i64 %85, %82
  %87 = add i64 %86, %84
  %88 = getelementptr i64, ptr %76, i64 %87
  store i64 %75, ptr %88, align 4
  %89 = add i64 %59, 1
  br label %58

90:                                               ; preds = %58
  %91 = add i64 %53, 1
  br label %52
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

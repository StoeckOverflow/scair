; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @blocked_pack(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, i64 %11, i64 %12, i64 %13) {
  %15 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %9, 0
  %16 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %15, ptr %10, 1
  %17 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %16, i64 %11, 2
  %18 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %17, i64 %12, 3, 0
  %19 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %18, i64 %13, 4, 0
  %20 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %4, 0
  %21 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %20, ptr %5, 1
  %22 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %21, i64 %6, 2
  %23 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %22, i64 %7, 3, 0
  %24 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %23, i64 %8, 4, 0
  %25 = mul i64 %0, %2
  %26 = mul i64 %1, %3
  br label %27

27:                                               ; preds = %69, %14
  %28 = phi i64 [ %70, %69 ], [ 0, %14 ]
  %29 = icmp slt i64 %28, %0
  br i1 %29, label %30, label %71

30:                                               ; preds = %27
  %31 = mul i64 %28, %2
  %32 = sub i64 %25, %31
  %33 = call i64 @llvm.smin.i64(i64 %2, i64 %32)
  br label %34

34:                                               ; preds = %67, %30
  %35 = phi i64 [ %68, %67 ], [ 0, %30 ]
  %36 = icmp slt i64 %35, %1
  br i1 %36, label %37, label %69

37:                                               ; preds = %34
  %38 = mul i64 %35, %3
  %39 = sub i64 %26, %38
  %40 = call i64 @llvm.smin.i64(i64 %3, i64 %39)
  br label %41

41:                                               ; preds = %65, %37
  %42 = phi i64 [ %66, %65 ], [ 0, %37 ]
  %43 = icmp slt i64 %42, %33
  br i1 %43, label %44, label %67

44:                                               ; preds = %41
  %45 = add i64 %31, %42
  br label %46

46:                                               ; preds = %49, %44
  %47 = phi i64 [ %64, %49 ], [ 0, %44 ]
  %48 = icmp slt i64 %47, %40
  br i1 %48, label %49, label %65

49:                                               ; preds = %46
  %50 = add i64 %38, %47
  %51 = mul i64 %45, %26
  %52 = add i64 %51, %50
  %53 = mul i64 %28, %1
  %54 = add i64 %53, %35
  %55 = mul i64 %54, %2
  %56 = add i64 %55, %42
  %57 = mul i64 %56, %3
  %58 = add i64 %57, %47
  %59 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, 1
  %60 = getelementptr inbounds nuw i64, ptr %59, i64 %52
  %61 = load i64, ptr %60, align 4
  %62 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 1
  %63 = getelementptr inbounds nuw i64, ptr %62, i64 %58
  store i64 %61, ptr %63, align 4
  %64 = add i64 %47, 1
  br label %46

65:                                               ; preds = %46
  %66 = add i64 %42, 1
  br label %41

67:                                               ; preds = %41
  %68 = add i64 %35, 1
  br label %34

69:                                               ; preds = %34
  %70 = add i64 %28, 1
  br label %27

71:                                               ; preds = %27
  ret void
}

define void @_mlir_ciface_blocked_pack(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5) {
  %7 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %4, align 8
  %8 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 0
  %9 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 1
  %10 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 2
  %11 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 3, 0
  %12 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 4, 0
  %13 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %5, align 8
  %14 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 0
  %15 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 1
  %16 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 2
  %17 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 3, 0
  %18 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 4, 0
  call void @blocked_pack(i64 %0, i64 %1, i64 %2, i64 %3, ptr %8, ptr %9, i64 %10, i64 %11, i64 %12, ptr %14, ptr %15, i64 %16, i64 %17, i64 %18)
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.smin.i64(i64, i64) #0

attributes #0 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

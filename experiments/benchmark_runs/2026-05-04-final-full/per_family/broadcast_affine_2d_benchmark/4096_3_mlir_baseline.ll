; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @broadcast_affine_2d(i64 %0, i64 %1, ptr %2, ptr %3, i64 %4, i64 %5, i64 %6, ptr %7, ptr %8, i64 %9, i64 %10, i64 %11, ptr %12, ptr %13, i64 %14, i64 %15, i64 %16, ptr %17, ptr %18, i64 %19, i64 %20, i64 %21) {
  %23 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %17, 0
  %24 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %23, ptr %18, 1
  %25 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, i64 %19, 2
  %26 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, i64 %20, 3, 0
  %27 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %26, i64 %21, 4, 0
  %28 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %12, 0
  %29 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %28, ptr %13, 1
  %30 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %29, i64 %14, 2
  %31 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, i64 %15, 3, 0
  %32 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %31, i64 %16, 4, 0
  %33 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %7, 0
  %34 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %33, ptr %8, 1
  %35 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %34, i64 %9, 2
  %36 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %35, i64 %10, 3, 0
  %37 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %36, i64 %11, 4, 0
  %38 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %2, 0
  %39 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %38, ptr %3, 1
  %40 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %39, i64 %4, 2
  %41 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %40, i64 %5, 3, 0
  %42 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %41, i64 %6, 4, 0
  %43 = mul i64 %0, %1
  br label %44

44:                                               ; preds = %69, %22
  %45 = phi i64 [ %70, %69 ], [ 0, %22 ]
  %46 = icmp slt i64 %45, %43
  br i1 %46, label %47, label %71

47:                                               ; preds = %44
  %48 = sub i64 %43, %45
  %49 = call i64 @llvm.smin.i64(i64 %1, i64 %48)
  br label %50

50:                                               ; preds = %53, %47
  %51 = phi i64 [ %68, %53 ], [ 0, %47 ]
  %52 = icmp slt i64 %51, %49
  br i1 %52, label %53, label %69

53:                                               ; preds = %50
  %54 = add i64 %45, %51
  %55 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %42, 1
  %56 = getelementptr inbounds nuw i64, ptr %55, i64 %54
  %57 = load i64, ptr %56, align 4
  %58 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %37, 1
  %59 = getelementptr inbounds nuw i64, ptr %58, i64 %51
  %60 = load i64, ptr %59, align 4
  %61 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %32, 1
  %62 = getelementptr inbounds nuw i64, ptr %61, i64 %51
  %63 = load i64, ptr %62, align 4
  %64 = mul i64 %57, %60
  %65 = add i64 %64, %63
  %66 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %27, 1
  %67 = getelementptr inbounds nuw i64, ptr %66, i64 %54
  store i64 %65, ptr %67, align 4
  %68 = add i64 %51, 1
  br label %50

69:                                               ; preds = %50
  %70 = add i64 %45, %1
  br label %44

71:                                               ; preds = %44
  ret void
}

define void @_mlir_ciface_broadcast_affine_2d(i64 %0, i64 %1, ptr %2, ptr %3, ptr %4, ptr %5) {
  %7 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %2, align 8
  %8 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 0
  %9 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 1
  %10 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 2
  %11 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 3, 0
  %12 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %7, 4, 0
  %13 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %3, align 8
  %14 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 0
  %15 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 1
  %16 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 2
  %17 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 3, 0
  %18 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 4, 0
  %19 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %4, align 8
  %20 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 0
  %21 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 1
  %22 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 2
  %23 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 3, 0
  %24 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 4, 0
  %25 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %5, align 8
  %26 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 0
  %27 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 1
  %28 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 2
  %29 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 3, 0
  %30 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 4, 0
  call void @broadcast_affine_2d(i64 %0, i64 %1, ptr %8, ptr %9, i64 %10, i64 %11, i64 %12, ptr %14, ptr %15, i64 %16, i64 %17, i64 %18, ptr %20, ptr %21, i64 %22, i64 %23, i64 %24, ptr %26, ptr %27, i64 %28, i64 %29, i64 %30)
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.smin.i64(i64, i64) #0

attributes #0 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

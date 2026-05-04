; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @semi_affine_fill_and_sum(i64 %0, i64 %1, ptr %2, ptr %3, i64 %4, i64 %5, i64 %6, ptr %7, ptr %8, i64 %9, i64 %10, i64 %11) {
  %13 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %7, 0
  %14 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, ptr %8, 1
  %15 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, i64 %9, 2
  %16 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %15, i64 %10, 3, 0
  %17 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %16, i64 %11, 4, 0
  %18 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %2, 0
  %19 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %18, ptr %3, 1
  %20 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, i64 %4, 2
  %21 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %20, i64 %5, 3, 0
  %22 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %21, i64 %6, 4, 0
  %23 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %22, 0
  %24 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %22, 1
  %25 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } poison, ptr %23, 0
  %26 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %25, ptr %24, 1
  %27 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %26, i64 0, 2
  %28 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %27, i64 256, 3, 0
  %29 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %28, i64 %0, 4, 0
  %30 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %29, i64 1024, 3, 1
  %31 = insertvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %30, i64 %1, 4, 1
  br label %32

32:                                               ; preds = %48, %12
  %33 = phi i64 [ %49, %48 ], [ 0, %12 ]
  %34 = icmp slt i64 %33, 256
  br i1 %34, label %35, label %50

35:                                               ; preds = %32
  br label %36

36:                                               ; preds = %39, %35
  %37 = phi i64 [ %47, %39 ], [ 0, %35 ]
  %38 = icmp slt i64 %37, 1024
  br i1 %38, label %39, label %48

39:                                               ; preds = %36
  %40 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %31, 1
  %41 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %31, 4, 0
  %42 = mul nuw nsw i64 %33, %41
  %43 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %31, 4, 1
  %44 = mul nuw nsw i64 %37, %43
  %45 = add nuw nsw i64 %42, %44
  %46 = getelementptr inbounds nuw float, ptr %40, i64 %45
  store float 1.000000e+00, ptr %46, align 4
  %47 = add i64 %37, 1
  br label %36

48:                                               ; preds = %36
  %49 = add i64 %33, 1
  br label %32

50:                                               ; preds = %32
  br label %51

51:                                               ; preds = %71, %50
  %52 = phi i64 [ %72, %71 ], [ 0, %50 ]
  %53 = phi float [ %58, %71 ], [ 0.000000e+00, %50 ]
  %54 = icmp slt i64 %52, 256
  br i1 %54, label %55, label %73

55:                                               ; preds = %51
  br label %56

56:                                               ; preds = %60, %55
  %57 = phi i64 [ %70, %60 ], [ 0, %55 ]
  %58 = phi float [ %69, %60 ], [ %53, %55 ]
  %59 = icmp slt i64 %57, 1024
  br i1 %59, label %60, label %71

60:                                               ; preds = %56
  %61 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %31, 1
  %62 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %31, 4, 0
  %63 = mul nuw nsw i64 %52, %62
  %64 = extractvalue { ptr, ptr, i64, [2 x i64], [2 x i64] } %31, 4, 1
  %65 = mul nuw nsw i64 %57, %64
  %66 = add nuw nsw i64 %63, %65
  %67 = getelementptr inbounds nuw float, ptr %61, i64 %66
  %68 = load float, ptr %67, align 4
  %69 = fadd float %58, %68
  %70 = add i64 %57, 1
  br label %56

71:                                               ; preds = %56
  %72 = add i64 %52, 1
  br label %51

73:                                               ; preds = %51
  %74 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %17, 1
  %75 = getelementptr inbounds nuw float, ptr %74, i64 0
  store float %53, ptr %75, align 4
  ret void
}

define void @_mlir_ciface_semi_affine_fill_and_sum(i64 %0, i64 %1, ptr %2, ptr %3) {
  %5 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %2, align 8
  %6 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %5, 0
  %7 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %5, 1
  %8 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %5, 2
  %9 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %5, 3, 0
  %10 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %5, 4, 0
  %11 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %3, align 8
  %12 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %11, 0
  %13 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %11, 1
  %14 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %11, 2
  %15 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %11, 3, 0
  %16 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %11, 4, 0
  call void @semi_affine_fill_and_sum(i64 %0, i64 %1, ptr %6, ptr %7, i64 %8, i64 %9, i64 %10, ptr %12, ptr %13, i64 %14, i64 %15, i64 %16)
  ret void
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @conv2d_dynamic(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, ptr %11) {
  %13 = mul i64 %2, %3
  %14 = mul i64 %1, %13
  %15 = mul i64 %5, %6
  %16 = mul i64 %1, %15
  %17 = mul i64 %7, %8
  %18 = mul i64 %4, %17
  br label %19

19:                                               ; preds = %28, %12
  %20 = phi i64 [ %29, %28 ], [ 0, %12 ]
  %21 = icmp slt i64 %20, %0
  br i1 %21, label %22, label %23

22:                                               ; preds = %19
  br label %24

23:                                               ; preds = %19
  ret void

24:                                               ; preds = %34, %22
  %25 = phi i64 [ %35, %34 ], [ 0, %22 ]
  %26 = icmp slt i64 %25, %4
  br i1 %26, label %27, label %28

27:                                               ; preds = %24
  br label %30

28:                                               ; preds = %24
  %29 = add i64 %20, 1
  br label %19

30:                                               ; preds = %40, %27
  %31 = phi i64 [ %41, %40 ], [ 0, %27 ]
  %32 = icmp slt i64 %31, %7
  br i1 %32, label %33, label %34

33:                                               ; preds = %30
  br label %36

34:                                               ; preds = %30
  %35 = add i64 %25, 1
  br label %24

36:                                               ; preds = %55, %33
  %37 = phi i64 [ %63, %55 ], [ 0, %33 ]
  %38 = icmp slt i64 %37, %8
  br i1 %38, label %39, label %40

39:                                               ; preds = %36
  br label %42

40:                                               ; preds = %36
  %41 = add i64 %31, 1
  br label %30

42:                                               ; preds = %53, %39
  %43 = phi i64 [ %54, %53 ], [ 0, %39 ]
  %44 = phi float [ %50, %53 ], [ 0.000000e+00, %39 ]
  %45 = icmp slt i64 %43, %1
  br i1 %45, label %46, label %55

46:                                               ; preds = %42
  br label %47

47:                                               ; preds = %91, %46
  %48 = phi i64 [ %48, %91 ], [ %43, %46 ]
  %49 = phi i64 [ %92, %91 ], [ 0, %46 ]
  %50 = phi float [ %66, %91 ], [ %44, %46 ]
  %51 = icmp slt i64 %49, %5
  br i1 %51, label %52, label %53

52:                                               ; preds = %47
  br label %64

53:                                               ; preds = %47
  %54 = add i64 %48, 1
  br label %42

55:                                               ; preds = %42
  %56 = mul i64 %20, %18
  %57 = mul i64 %25, %17
  %58 = mul i64 %31, %8
  %59 = add i64 %56, %57
  %60 = add i64 %59, %58
  %61 = add i64 %60, %37
  %62 = getelementptr float, ptr %11, i64 %61
  store float %44, ptr %62, align 4
  %63 = add i64 %37, 1
  br label %36

64:                                               ; preds = %68, %52
  %65 = phi i64 [ %90, %68 ], [ 0, %52 ]
  %66 = phi float [ %89, %68 ], [ %50, %52 ]
  %67 = icmp slt i64 %65, %6
  br i1 %67, label %68, label %91

68:                                               ; preds = %64
  %69 = mul i64 %20, %14
  %70 = mul i64 %48, %13
  %71 = mul i64 %31, %3
  %72 = mul i64 %49, %3
  %73 = add i64 %69, %70
  %74 = add i64 %73, %71
  %75 = add i64 %74, %37
  %76 = add i64 %75, %72
  %77 = add i64 %76, %65
  %78 = getelementptr float, ptr %9, i64 %77
  %79 = load float, ptr %78, align 4
  %80 = mul i64 %25, %16
  %81 = mul i64 %48, %15
  %82 = mul i64 %49, %6
  %83 = add i64 %80, %81
  %84 = add i64 %83, %82
  %85 = add i64 %84, %65
  %86 = getelementptr float, ptr %10, i64 %85
  %87 = load float, ptr %86, align 4
  %88 = fmul float %79, %87
  %89 = fadd float %66, %88
  %90 = add i64 %65, 1
  br label %64

91:                                               ; preds = %64
  %92 = add i64 %49, 1
  br label %47
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

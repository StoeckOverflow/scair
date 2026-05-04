; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @matmul_strided(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, ptr %11) {
  %13 = add i64 %3, 0
  %14 = add i64 %4, 0
  %15 = add i64 %5, 0
  %16 = add i64 %6, 0
  %17 = add i64 %7, 0
  %18 = add i64 %8, 0
  br label %19

19:                                               ; preds = %28, %12
  %20 = phi i64 [ %29, %28 ], [ 0, %12 ]
  %21 = icmp slt i64 %20, %0
  br i1 %21, label %22, label %23

22:                                               ; preds = %19
  br label %24

23:                                               ; preds = %19
  ret void

24:                                               ; preds = %48, %22
  %25 = phi i64 [ %53, %48 ], [ 0, %22 ]
  %26 = icmp slt i64 %25, %1
  br i1 %26, label %27, label %28

27:                                               ; preds = %24
  br label %30

28:                                               ; preds = %24
  %29 = add i64 %20, 1
  br label %19

30:                                               ; preds = %34, %27
  %31 = phi i64 [ %47, %34 ], [ 0, %27 ]
  %32 = phi float [ %46, %34 ], [ 0.000000e+00, %27 ]
  %33 = icmp slt i64 %31, %2
  br i1 %33, label %34, label %48

34:                                               ; preds = %30
  %35 = mul i64 %20, %13
  %36 = mul i64 %31, %14
  %37 = add i64 %35, %36
  %38 = getelementptr float, ptr %9, i64 %37
  %39 = load float, ptr %38, align 4
  %40 = mul i64 %31, %15
  %41 = mul i64 %25, %16
  %42 = add i64 %40, %41
  %43 = getelementptr float, ptr %10, i64 %42
  %44 = load float, ptr %43, align 4
  %45 = fmul float %39, %44
  %46 = fadd float %32, %45
  %47 = add i64 %31, 1
  br label %30

48:                                               ; preds = %30
  %49 = mul i64 %20, %17
  %50 = mul i64 %25, %18
  %51 = add i64 %49, %50
  %52 = getelementptr float, ptr %11, i64 %51
  store float %32, ptr %52, align 4
  %53 = add i64 %25, 1
  br label %24
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

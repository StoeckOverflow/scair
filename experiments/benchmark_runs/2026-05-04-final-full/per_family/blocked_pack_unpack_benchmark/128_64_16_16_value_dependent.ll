; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @blocked_pack(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5) {
  %7 = mul i64 %0, %2
  %8 = mul i64 %1, %3
  %9 = mul i64 %3, 1
  %10 = mul i64 %2, %3
  %11 = mul i64 %1, %10
  br label %12

12:                                               ; preds = %23, %6
  %13 = phi i64 [ %24, %23 ], [ 0, %6 ]
  %14 = icmp slt i64 %13, %0
  br i1 %14, label %15, label %17

15:                                               ; preds = %12
  %16 = mul i64 %13, %2
  br label %18

17:                                               ; preds = %12
  ret void

18:                                               ; preds = %30, %15
  %19 = phi i64 [ %31, %30 ], [ 0, %15 ]
  %20 = icmp slt i64 %19, %1
  br i1 %20, label %21, label %23

21:                                               ; preds = %18
  %22 = mul i64 %19, %3
  br label %25

23:                                               ; preds = %18
  %24 = add i64 %13, 1
  br label %12

25:                                               ; preds = %49, %21
  %26 = phi i64 [ %50, %49 ], [ 0, %21 ]
  %27 = icmp slt i64 %26, %2
  br i1 %27, label %28, label %30

28:                                               ; preds = %25
  %29 = add i64 %16, %26
  br label %32

30:                                               ; preds = %25
  %31 = add i64 %19, 1
  br label %18

32:                                               ; preds = %35, %28
  %33 = phi i64 [ %48, %35 ], [ 0, %28 ]
  %34 = icmp slt i64 %33, %3
  br i1 %34, label %35, label %49

35:                                               ; preds = %32
  %36 = add i64 %22, %33
  %37 = mul i64 %29, %8
  %38 = add i64 %37, %36
  %39 = getelementptr i64, ptr %4, i64 %38
  %40 = load i64, ptr %39, align 4
  %41 = mul i64 %13, %11
  %42 = mul i64 %19, %10
  %43 = mul i64 %26, %9
  %44 = add i64 %41, %42
  %45 = add i64 %44, %43
  %46 = add i64 %45, %33
  %47 = getelementptr i64, ptr %5, i64 %46
  store i64 %40, ptr %47, align 4
  %48 = add i64 %33, 1
  br label %32

49:                                               ; preds = %32
  %50 = add i64 %26, 1
  br label %25
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

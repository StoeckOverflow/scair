; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @conv2d_dynamic(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, ptr %11) {
  %13 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %9, align 8
  %14 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %10, align 8
  %15 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %11, align 8
  %16 = mul i64 %2, %3
  %17 = mul i64 %1, %16
  %18 = mul i64 %5, %6
  %19 = mul i64 %1, %18
  %20 = mul i64 %7, %8
  %21 = mul i64 %4, %20
  %22 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 0
  %23 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 1
  %24 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } poison, ptr %22, 0
  %25 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %24, ptr %23, 1
  %26 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %25, i64 0, 2
  %27 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %26, i64 %0, 3, 0
  %28 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %27, i64 %1, 3, 1
  %29 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %28, i64 %7, 3, 2
  %30 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %29, i64 %8, 3, 3
  %31 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %30, i64 %5, 3, 4
  %32 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %31, i64 %6, 3, 5
  %33 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %32, i64 %17, 4, 0
  %34 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %33, i64 %16, 4, 1
  %35 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %34, i64 %3, 4, 2
  %36 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %35, i64 1, 4, 3
  %37 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %36, i64 %3, 4, 4
  %38 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %37, i64 1, 4, 5
  %39 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 0
  %40 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 1
  %41 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %39, 0
  %42 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %41, ptr %40, 1
  %43 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %42, i64 0, 2
  %44 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %43, i64 %4, 3, 0
  %45 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %44, i64 %1, 3, 1
  %46 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %45, i64 %5, 3, 2
  %47 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %46, i64 %6, 3, 3
  %48 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %47, i64 %19, 4, 0
  %49 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %48, i64 %18, 4, 1
  %50 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %49, i64 %6, 4, 2
  %51 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %50, i64 1, 4, 3
  %52 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %15, 0
  %53 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %15, 1
  %54 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %52, 0
  %55 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %54, ptr %53, 1
  %56 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %55, i64 0, 2
  %57 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %56, i64 %0, 3, 0
  %58 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %57, i64 %4, 3, 1
  %59 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %58, i64 %7, 3, 2
  %60 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %59, i64 %8, 3, 3
  %61 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %60, i64 %21, 4, 0
  %62 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %61, i64 %20, 4, 1
  %63 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %62, i64 %8, 4, 2
  %64 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %63, i64 1, 4, 3
  br label %65

65:                                               ; preds = %74, %12
  %66 = phi i64 [ %75, %74 ], [ 0, %12 ]
  %67 = icmp slt i64 %66, %0
  br i1 %67, label %68, label %69

68:                                               ; preds = %65
  br label %70

69:                                               ; preds = %65
  ret void

70:                                               ; preds = %80, %68
  %71 = phi i64 [ %81, %80 ], [ 0, %68 ]
  %72 = icmp slt i64 %71, %4
  br i1 %72, label %73, label %74

73:                                               ; preds = %70
  br label %76

74:                                               ; preds = %70
  %75 = add i64 %66, 1
  br label %65

76:                                               ; preds = %86, %73
  %77 = phi i64 [ %87, %86 ], [ 0, %73 ]
  %78 = icmp slt i64 %77, %7
  br i1 %78, label %79, label %80

79:                                               ; preds = %76
  br label %82

80:                                               ; preds = %76
  %81 = add i64 %71, 1
  br label %70

82:                                               ; preds = %100, %79
  %83 = phi i64 [ %114, %100 ], [ 0, %79 ]
  %84 = icmp slt i64 %83, %8
  br i1 %84, label %85, label %86

85:                                               ; preds = %82
  br label %88

86:                                               ; preds = %82
  %87 = add i64 %77, 1
  br label %76

88:                                               ; preds = %98, %85
  %89 = phi i64 [ %99, %98 ], [ 0, %85 ]
  %90 = phi float [ %95, %98 ], [ 0.000000e+00, %85 ]
  %91 = icmp slt i64 %89, %1
  br i1 %91, label %92, label %100

92:                                               ; preds = %88
  br label %93

93:                                               ; preds = %157, %92
  %94 = phi i64 [ %158, %157 ], [ 0, %92 ]
  %95 = phi float [ %117, %157 ], [ %90, %92 ]
  %96 = icmp slt i64 %94, %5
  br i1 %96, label %97, label %98

97:                                               ; preds = %93
  br label %115

98:                                               ; preds = %93
  %99 = add i64 %89, 1
  br label %88

100:                                              ; preds = %88
  %101 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %64, 1
  %102 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %64, 4, 0
  %103 = mul i64 %66, %102
  %104 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %64, 4, 1
  %105 = mul i64 %71, %104
  %106 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %64, 4, 2
  %107 = mul i64 %77, %106
  %108 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %64, 4, 3
  %109 = mul i64 %83, %108
  %110 = add i64 %103, %105
  %111 = add i64 %110, %107
  %112 = add i64 %111, %109
  %113 = getelementptr float, ptr %101, i64 %112
  store float %90, ptr %113, align 4
  %114 = add i64 %83, 1
  br label %82

115:                                              ; preds = %119, %97
  %116 = phi i64 [ %156, %119 ], [ 0, %97 ]
  %117 = phi float [ %155, %119 ], [ %95, %97 ]
  %118 = icmp slt i64 %116, %6
  br i1 %118, label %119, label %157

119:                                              ; preds = %115
  %120 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %38, 1
  %121 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %38, 4, 0
  %122 = mul i64 %66, %121
  %123 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %38, 4, 1
  %124 = mul i64 %89, %123
  %125 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %38, 4, 2
  %126 = mul i64 %77, %125
  %127 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %38, 4, 3
  %128 = mul i64 %83, %127
  %129 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %38, 4, 4
  %130 = mul i64 %94, %129
  %131 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %38, 4, 5
  %132 = mul i64 %116, %131
  %133 = add i64 %122, %124
  %134 = add i64 %133, %126
  %135 = add i64 %134, %128
  %136 = add i64 %135, %130
  %137 = add i64 %136, %132
  %138 = getelementptr float, ptr %120, i64 %137
  %139 = load float, ptr %138, align 4
  %140 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %51, 1
  %141 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %51, 4, 0
  %142 = mul i64 %71, %141
  %143 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %51, 4, 1
  %144 = mul i64 %89, %143
  %145 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %51, 4, 2
  %146 = mul i64 %94, %145
  %147 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %51, 4, 3
  %148 = mul i64 %116, %147
  %149 = add i64 %142, %144
  %150 = add i64 %149, %146
  %151 = add i64 %150, %148
  %152 = getelementptr float, ptr %140, i64 %151
  %153 = load float, ptr %152, align 4
  %154 = fmul float %139, %153
  %155 = fadd float %117, %154
  %156 = add i64 %116, 1
  br label %115

157:                                              ; preds = %115
  %158 = add i64 %94, 1
  br label %93
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

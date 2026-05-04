; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

define void @conv2d_dynamic(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, i64 %11, i64 %12, i64 %13, ptr %14, ptr %15, i64 %16, i64 %17, i64 %18, ptr %19, ptr %20, i64 %21, i64 %22, i64 %23) {
  %25 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %19, 0
  %26 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, ptr %20, 1
  %27 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %26, i64 %21, 2
  %28 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %27, i64 %22, 3, 0
  %29 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %28, i64 %23, 4, 0
  %30 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %14, 0
  %31 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, ptr %15, 1
  %32 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %31, i64 %16, 2
  %33 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %32, i64 %17, 3, 0
  %34 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %33, i64 %18, 4, 0
  %35 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %9, 0
  %36 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %35, ptr %10, 1
  %37 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %36, i64 %11, 2
  %38 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %37, i64 %12, 3, 0
  %39 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %38, i64 %13, 4, 0
  %40 = mul i64 %2, %3
  %41 = mul i64 %1, %40
  %42 = mul i64 %5, %6
  %43 = mul i64 %1, %42
  %44 = mul i64 %7, %8
  %45 = mul i64 %4, %44
  %46 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %39, 0
  %47 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %39, 1
  %48 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } poison, ptr %46, 0
  %49 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %48, ptr %47, 1
  %50 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %49, i64 0, 2
  %51 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %50, i64 %0, 3, 0
  %52 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %51, i64 %41, 4, 0
  %53 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %52, i64 %1, 3, 1
  %54 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %53, i64 %40, 4, 1
  %55 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %54, i64 %7, 3, 2
  %56 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %55, i64 %3, 4, 2
  %57 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %56, i64 %8, 3, 3
  %58 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %57, i64 1, 4, 3
  %59 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %58, i64 %5, 3, 4
  %60 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %59, i64 %3, 4, 4
  %61 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %60, i64 %6, 3, 5
  %62 = insertvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %61, i64 1, 4, 5
  %63 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %34, 0
  %64 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %34, 1
  %65 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %63, 0
  %66 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %65, ptr %64, 1
  %67 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %66, i64 0, 2
  %68 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %67, i64 %4, 3, 0
  %69 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %68, i64 %43, 4, 0
  %70 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %69, i64 %1, 3, 1
  %71 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %70, i64 %42, 4, 1
  %72 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %71, i64 %5, 3, 2
  %73 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %72, i64 %6, 4, 2
  %74 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %73, i64 %6, 3, 3
  %75 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, i64 1, 4, 3
  %76 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %29, 0
  %77 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %29, 1
  %78 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %76, 0
  %79 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %78, ptr %77, 1
  %80 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %79, i64 0, 2
  %81 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %80, i64 %0, 3, 0
  %82 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %81, i64 %45, 4, 0
  %83 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %82, i64 %4, 3, 1
  %84 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %83, i64 %44, 4, 1
  %85 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %84, i64 %7, 3, 2
  %86 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %85, i64 %8, 4, 2
  %87 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %86, i64 %8, 3, 3
  %88 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, i64 1, 4, 3
  br label %89

89:                                               ; preds = %186, %24
  %90 = phi i64 [ %187, %186 ], [ 0, %24 ]
  %91 = icmp slt i64 %90, %0
  br i1 %91, label %92, label %188

92:                                               ; preds = %89
  br label %93

93:                                               ; preds = %184, %92
  %94 = phi i64 [ %185, %184 ], [ 0, %92 ]
  %95 = icmp slt i64 %94, %4
  br i1 %95, label %96, label %186

96:                                               ; preds = %93
  br label %97

97:                                               ; preds = %182, %96
  %98 = phi i64 [ %183, %182 ], [ 0, %96 ]
  %99 = icmp slt i64 %98, %7
  br i1 %99, label %100, label %184

100:                                              ; preds = %97
  br label %101

101:                                              ; preds = %165, %100
  %102 = phi i64 [ %181, %165 ], [ 0, %100 ]
  %103 = icmp slt i64 %102, %8
  br i1 %103, label %104, label %182

104:                                              ; preds = %101
  br label %105

105:                                              ; preds = %163, %104
  %106 = phi i64 [ %164, %163 ], [ 0, %104 ]
  %107 = phi float [ %112, %163 ], [ 0.000000e+00, %104 ]
  %108 = icmp slt i64 %106, %1
  br i1 %108, label %109, label %165

109:                                              ; preds = %105
  br label %110

110:                                              ; preds = %161, %109
  %111 = phi i64 [ %162, %161 ], [ 0, %109 ]
  %112 = phi float [ %117, %161 ], [ %107, %109 ]
  %113 = icmp slt i64 %111, %5
  br i1 %113, label %114, label %163

114:                                              ; preds = %110
  br label %115

115:                                              ; preds = %119, %114
  %116 = phi i64 [ %160, %119 ], [ 0, %114 ]
  %117 = phi float [ %159, %119 ], [ %112, %114 ]
  %118 = icmp slt i64 %116, %6
  br i1 %118, label %119, label %161

119:                                              ; preds = %115
  %120 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %62, 1
  %121 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %62, 2
  %122 = getelementptr float, ptr %120, i64 %121
  %123 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %62, 4, 0
  %124 = mul nuw nsw i64 %90, %123
  %125 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %62, 4, 1
  %126 = mul nuw nsw i64 %106, %125
  %127 = add nuw nsw i64 %124, %126
  %128 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %62, 4, 2
  %129 = mul nuw nsw i64 %98, %128
  %130 = add nuw nsw i64 %127, %129
  %131 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %62, 4, 3
  %132 = mul nuw nsw i64 %102, %131
  %133 = add nuw nsw i64 %130, %132
  %134 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %62, 4, 4
  %135 = mul nuw nsw i64 %111, %134
  %136 = add nuw nsw i64 %133, %135
  %137 = extractvalue { ptr, ptr, i64, [6 x i64], [6 x i64] } %62, 4, 5
  %138 = mul nuw nsw i64 %116, %137
  %139 = add nuw nsw i64 %136, %138
  %140 = getelementptr inbounds nuw float, ptr %122, i64 %139
  %141 = load float, ptr %140, align 4
  %142 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %75, 1
  %143 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %75, 2
  %144 = getelementptr float, ptr %142, i64 %143
  %145 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %75, 4, 0
  %146 = mul nuw nsw i64 %94, %145
  %147 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %75, 4, 1
  %148 = mul nuw nsw i64 %106, %147
  %149 = add nuw nsw i64 %146, %148
  %150 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %75, 4, 2
  %151 = mul nuw nsw i64 %111, %150
  %152 = add nuw nsw i64 %149, %151
  %153 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %75, 4, 3
  %154 = mul nuw nsw i64 %116, %153
  %155 = add nuw nsw i64 %152, %154
  %156 = getelementptr inbounds nuw float, ptr %144, i64 %155
  %157 = load float, ptr %156, align 4
  %158 = fmul float %141, %157
  %159 = fadd float %117, %158
  %160 = add i64 %116, 1
  br label %115

161:                                              ; preds = %115
  %162 = add i64 %111, 1
  br label %110

163:                                              ; preds = %110
  %164 = add i64 %106, 1
  br label %105

165:                                              ; preds = %105
  %166 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %88, 1
  %167 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %88, 2
  %168 = getelementptr float, ptr %166, i64 %167
  %169 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %88, 4, 0
  %170 = mul nuw nsw i64 %90, %169
  %171 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %88, 4, 1
  %172 = mul nuw nsw i64 %94, %171
  %173 = add nuw nsw i64 %170, %172
  %174 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %88, 4, 2
  %175 = mul nuw nsw i64 %98, %174
  %176 = add nuw nsw i64 %173, %175
  %177 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %88, 4, 3
  %178 = mul nuw nsw i64 %102, %177
  %179 = add nuw nsw i64 %176, %178
  %180 = getelementptr inbounds nuw float, ptr %168, i64 %179
  store float %107, ptr %180, align 4
  %181 = add i64 %102, 1
  br label %101

182:                                              ; preds = %101
  %183 = add i64 %98, 1
  br label %97

184:                                              ; preds = %97
  %185 = add i64 %94, 1
  br label %93

186:                                              ; preds = %93
  %187 = add i64 %90, 1
  br label %89

188:                                              ; preds = %89
  ret void
}

define void @_mlir_ciface_conv2d_dynamic(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, ptr %11) {
  %13 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %9, align 8
  %14 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 0
  %15 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 1
  %16 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 2
  %17 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 3, 0
  %18 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 4, 0
  %19 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %10, align 8
  %20 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 0
  %21 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 1
  %22 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 2
  %23 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 3, 0
  %24 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %19, 4, 0
  %25 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %11, align 8
  %26 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 0
  %27 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 1
  %28 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 2
  %29 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 3, 0
  %30 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %25, 4, 0
  call void @conv2d_dynamic(i64 %0, i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6, i64 %7, i64 %8, ptr %14, ptr %15, i64 %16, i64 %17, i64 %18, ptr %20, ptr %21, i64 %22, i64 %23, i64 %24, ptr %26, ptr %27, i64 %28, i64 %29, i64 %30)
  ret void
}

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

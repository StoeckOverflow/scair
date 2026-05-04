; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

declare float @bench_expf(float)

declare float @bench_inv_sqrt_index(i64)

define void @attention_mha(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8, ptr %9, ptr %10) {
  %12 = mul i64 %2, %3
  %13 = mul i64 %1, %12
  %14 = mul i64 %1, %1
  %15 = mul i64 %2, %14
  %16 = call float @bench_inv_sqrt_index(i64 %3)
  br label %17

17:                                               ; preds = %27, %11
  %18 = phi i64 [ %28, %27 ], [ 0, %11 ]
  %19 = icmp slt i64 %18, %0
  br i1 %19, label %20, label %21

20:                                               ; preds = %17
  br label %22

21:                                               ; preds = %17
  br label %72

22:                                               ; preds = %33, %20
  %23 = phi i64 [ %34, %33 ], [ 0, %20 ]
  %24 = icmp slt i64 %23, %2
  br i1 %24, label %25, label %27

25:                                               ; preds = %22
  %26 = mul i64 %23, %3
  br label %29

27:                                               ; preds = %22
  %28 = add i64 %18, 1
  br label %17

29:                                               ; preds = %39, %25
  %30 = phi i64 [ %40, %39 ], [ 0, %25 ]
  %31 = icmp slt i64 %30, %1
  br i1 %31, label %32, label %33

32:                                               ; preds = %29
  br label %35

33:                                               ; preds = %29
  %34 = add i64 %23, 1
  br label %22

35:                                               ; preds = %62, %32
  %36 = phi i64 [ %71, %62 ], [ 0, %32 ]
  %37 = icmp slt i64 %36, %1
  br i1 %37, label %38, label %39

38:                                               ; preds = %35
  br label %41

39:                                               ; preds = %35
  %40 = add i64 %30, 1
  br label %29

41:                                               ; preds = %45, %38
  %42 = phi i64 [ %61, %45 ], [ 0, %38 ]
  %43 = phi float [ %60, %45 ], [ 0.000000e+00, %38 ]
  %44 = icmp slt i64 %42, %3
  br i1 %44, label %45, label %62

45:                                               ; preds = %41
  %46 = add i64 %26, %42
  %47 = mul i64 %18, %13
  %48 = mul i64 %30, %12
  %49 = add i64 %47, %48
  %50 = add i64 %49, %46
  %51 = getelementptr float, ptr %4, i64 %50
  %52 = load float, ptr %51, align 4
  %53 = mul i64 %18, %13
  %54 = mul i64 %36, %12
  %55 = add i64 %53, %54
  %56 = add i64 %55, %46
  %57 = getelementptr float, ptr %5, i64 %56
  %58 = load float, ptr %57, align 4
  %59 = fmul float %52, %58
  %60 = fadd float %43, %59
  %61 = add i64 %42, 1
  br label %41

62:                                               ; preds = %41
  %63 = fmul float %43, %16
  %64 = mul i64 %18, %15
  %65 = mul i64 %23, %14
  %66 = mul i64 %30, %1
  %67 = add i64 %64, %65
  %68 = add i64 %67, %66
  %69 = add i64 %68, %36
  %70 = getelementptr float, ptr %7, i64 %69
  store float %63, ptr %70, align 4
  %71 = add i64 %36, 1
  br label %35

72:                                               ; preds = %81, %21
  %73 = phi i64 [ %82, %81 ], [ 0, %21 ]
  %74 = icmp slt i64 %73, %0
  br i1 %74, label %75, label %76

75:                                               ; preds = %72
  br label %77

76:                                               ; preds = %72
  br label %153

77:                                               ; preds = %87, %75
  %78 = phi i64 [ %88, %87 ], [ 0, %75 ]
  %79 = icmp slt i64 %78, %2
  br i1 %79, label %80, label %81

80:                                               ; preds = %77
  br label %83

81:                                               ; preds = %77
  %82 = add i64 %73, 1
  br label %72

83:                                               ; preds = %151, %80
  %84 = phi i64 [ %152, %151 ], [ 0, %80 ]
  %85 = icmp slt i64 %84, %1
  br i1 %85, label %86, label %87

86:                                               ; preds = %83
  br label %89

87:                                               ; preds = %83
  %88 = add i64 %78, 1
  br label %77

89:                                               ; preds = %93, %86
  %90 = phi i64 [ %103, %93 ], [ 0, %86 ]
  %91 = phi float [ %102, %93 ], [ 0xC7EFFFFFE0000000, %86 ]
  %92 = icmp slt i64 %90, %1
  br i1 %92, label %93, label %104

93:                                               ; preds = %89
  %94 = mul i64 %73, %15
  %95 = mul i64 %78, %14
  %96 = mul i64 %84, %1
  %97 = add i64 %94, %95
  %98 = add i64 %97, %96
  %99 = add i64 %98, %90
  %100 = getelementptr float, ptr %7, i64 %99
  %101 = load float, ptr %100, align 4
  %102 = call float @llvm.maximum.f32(float %91, float %101)
  %103 = add i64 %90, 1
  br label %89

104:                                              ; preds = %89
  br label %105

105:                                              ; preds = %109, %104
  %106 = phi i64 [ %128, %109 ], [ 0, %104 ]
  %107 = phi float [ %127, %109 ], [ 0.000000e+00, %104 ]
  %108 = icmp slt i64 %106, %1
  br i1 %108, label %109, label %129

109:                                              ; preds = %105
  %110 = mul i64 %73, %15
  %111 = mul i64 %78, %14
  %112 = mul i64 %84, %1
  %113 = add i64 %110, %111
  %114 = add i64 %113, %112
  %115 = add i64 %114, %106
  %116 = getelementptr float, ptr %7, i64 %115
  %117 = load float, ptr %116, align 4
  %118 = fsub float %117, %91
  %119 = call float @bench_expf(float %118)
  %120 = mul i64 %73, %15
  %121 = mul i64 %78, %14
  %122 = mul i64 %84, %1
  %123 = add i64 %120, %121
  %124 = add i64 %123, %122
  %125 = add i64 %124, %106
  %126 = getelementptr float, ptr %8, i64 %125
  store float %119, ptr %126, align 4
  %127 = fadd float %107, %119
  %128 = add i64 %106, 1
  br label %105

129:                                              ; preds = %105
  br label %130

130:                                              ; preds = %133, %129
  %131 = phi i64 [ %150, %133 ], [ 0, %129 ]
  %132 = icmp slt i64 %131, %1
  br i1 %132, label %133, label %151

133:                                              ; preds = %130
  %134 = mul i64 %73, %15
  %135 = mul i64 %78, %14
  %136 = mul i64 %84, %1
  %137 = add i64 %134, %135
  %138 = add i64 %137, %136
  %139 = add i64 %138, %131
  %140 = getelementptr float, ptr %8, i64 %139
  %141 = load float, ptr %140, align 4
  %142 = fdiv float %141, %107
  %143 = mul i64 %73, %15
  %144 = mul i64 %78, %14
  %145 = mul i64 %84, %1
  %146 = add i64 %143, %144
  %147 = add i64 %146, %145
  %148 = add i64 %147, %131
  %149 = getelementptr float, ptr %8, i64 %148
  store float %142, ptr %149, align 4
  %150 = add i64 %131, 1
  br label %130

151:                                              ; preds = %130
  %152 = add i64 %84, 1
  br label %83

153:                                              ; preds = %162, %76
  %154 = phi i64 [ %163, %162 ], [ 0, %76 ]
  %155 = icmp slt i64 %154, %0
  br i1 %155, label %156, label %157

156:                                              ; preds = %153
  br label %158

157:                                              ; preds = %153
  ret void

158:                                              ; preds = %233, %156
  %159 = phi i64 [ %234, %233 ], [ 0, %156 ]
  %160 = icmp slt i64 %159, %1
  br i1 %160, label %161, label %162

161:                                              ; preds = %158
  br label %164

162:                                              ; preds = %158
  %163 = add i64 %154, 1
  br label %153

164:                                              ; preds = %175, %161
  %165 = phi i64 [ %176, %175 ], [ 0, %161 ]
  %166 = icmp slt i64 %165, %2
  br i1 %166, label %167, label %169

167:                                              ; preds = %164
  %168 = mul i64 %165, %3
  br label %170

169:                                              ; preds = %164
  br label %206

170:                                              ; preds = %199, %167
  %171 = phi i64 [ %205, %199 ], [ 0, %167 ]
  %172 = icmp slt i64 %171, %3
  br i1 %172, label %173, label %175

173:                                              ; preds = %170
  %174 = add i64 %168, %171
  br label %177

175:                                              ; preds = %170
  %176 = add i64 %165, 1
  br label %164

177:                                              ; preds = %181, %173
  %178 = phi i64 [ %198, %181 ], [ 0, %173 ]
  %179 = phi float [ %197, %181 ], [ 0.000000e+00, %173 ]
  %180 = icmp slt i64 %178, %1
  br i1 %180, label %181, label %199

181:                                              ; preds = %177
  %182 = mul i64 %154, %15
  %183 = mul i64 %165, %14
  %184 = mul i64 %159, %1
  %185 = add i64 %182, %183
  %186 = add i64 %185, %184
  %187 = add i64 %186, %178
  %188 = getelementptr float, ptr %8, i64 %187
  %189 = load float, ptr %188, align 4
  %190 = mul i64 %154, %13
  %191 = mul i64 %178, %12
  %192 = add i64 %190, %191
  %193 = add i64 %192, %174
  %194 = getelementptr float, ptr %6, i64 %193
  %195 = load float, ptr %194, align 4
  %196 = fmul float %189, %195
  %197 = fadd float %179, %196
  %198 = add i64 %178, 1
  br label %177

199:                                              ; preds = %177
  %200 = mul i64 %154, %13
  %201 = mul i64 %159, %12
  %202 = add i64 %200, %201
  %203 = add i64 %202, %174
  %204 = getelementptr float, ptr %9, i64 %203
  store float %179, ptr %204, align 4
  %205 = add i64 %171, 1
  br label %170

206:                                              ; preds = %231, %169
  %207 = phi i64 [ %232, %231 ], [ 0, %169 ]
  %208 = phi float [ %214, %231 ], [ 0.000000e+00, %169 ]
  %209 = icmp slt i64 %207, %2
  br i1 %209, label %210, label %233

210:                                              ; preds = %206
  br label %211

211:                                              ; preds = %216, %210
  %212 = phi i64 [ %212, %216 ], [ %207, %210 ]
  %213 = phi i64 [ %230, %216 ], [ 0, %210 ]
  %214 = phi float [ %224, %216 ], [ %208, %210 ]
  %215 = icmp slt i64 %213, %3
  br i1 %215, label %216, label %231

216:                                              ; preds = %211
  %217 = mul i64 %212, %3
  %218 = add i64 %217, %213
  %219 = mul i64 %154, %13
  %220 = mul i64 %159, %12
  %221 = add i64 %219, %220
  %222 = add i64 %221, %218
  %223 = getelementptr float, ptr %9, i64 %222
  %224 = load float, ptr %223, align 4
  %225 = mul i64 %154, %13
  %226 = mul i64 %159, %12
  %227 = add i64 %225, %226
  %228 = add i64 %227, %218
  %229 = getelementptr float, ptr %10, i64 %228
  store float %224, ptr %229, align 4
  %230 = add i64 %213, 1
  br label %211

231:                                              ; preds = %211
  %232 = add i64 %212, 1
  br label %206

233:                                              ; preds = %206
  %234 = add i64 %159, 1
  br label %158
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.maximum.f32(float, float) #0

attributes #0 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

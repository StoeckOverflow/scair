; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

declare float @bench_expf(float)

declare float @bench_inv_sqrt_index(i64)

define void @attention_mha(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8, ptr %9, ptr %10) {
  %12 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %4, align 8
  %13 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %5, align 8
  %14 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %6, align 8
  %15 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %7, align 8
  %16 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %8, align 8
  %17 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %9, align 8
  %18 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %10, align 8
  %19 = mul i64 %2, %3
  %20 = mul i64 %1, %19
  %21 = mul i64 %1, %1
  %22 = mul i64 %2, %21
  %23 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %12, 0
  %24 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %12, 1
  %25 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %23, 0
  %26 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %25, ptr %24, 1
  %27 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %26, i64 0, 2
  %28 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %27, i64 %0, 3, 0
  %29 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %28, i64 %1, 3, 1
  %30 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %29, i64 %2, 3, 2
  %31 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %30, i64 %3, 3, 3
  %32 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %31, i64 %20, 4, 0
  %33 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %32, i64 %19, 4, 1
  %34 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %33, i64 %3, 4, 2
  %35 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %34, i64 1, 4, 3
  %36 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 0
  %37 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %13, 1
  %38 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %36, 0
  %39 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %38, ptr %37, 1
  %40 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %39, i64 0, 2
  %41 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %40, i64 %0, 3, 0
  %42 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %41, i64 %1, 3, 1
  %43 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %42, i64 %2, 3, 2
  %44 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %43, i64 %3, 3, 3
  %45 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %44, i64 %20, 4, 0
  %46 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %45, i64 %19, 4, 1
  %47 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %46, i64 %3, 4, 2
  %48 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %47, i64 1, 4, 3
  %49 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 0
  %50 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %14, 1
  %51 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %49, 0
  %52 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %51, ptr %50, 1
  %53 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %52, i64 0, 2
  %54 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %53, i64 %0, 3, 0
  %55 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %54, i64 %1, 3, 1
  %56 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %55, i64 %2, 3, 2
  %57 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %56, i64 %3, 3, 3
  %58 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %57, i64 %20, 4, 0
  %59 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %58, i64 %19, 4, 1
  %60 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %59, i64 %3, 4, 2
  %61 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %60, i64 1, 4, 3
  %62 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %15, 0
  %63 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %15, 1
  %64 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %62, 0
  %65 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %64, ptr %63, 1
  %66 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %65, i64 0, 2
  %67 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %66, i64 %0, 3, 0
  %68 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %67, i64 %2, 3, 1
  %69 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %68, i64 %1, 3, 2
  %70 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %69, i64 %1, 3, 3
  %71 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %70, i64 %22, 4, 0
  %72 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %71, i64 %21, 4, 1
  %73 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %72, i64 %1, 4, 2
  %74 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %73, i64 1, 4, 3
  %75 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %16, 0
  %76 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %16, 1
  %77 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %75, 0
  %78 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %77, ptr %76, 1
  %79 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %78, i64 0, 2
  %80 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %79, i64 %0, 3, 0
  %81 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %80, i64 %2, 3, 1
  %82 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %81, i64 %1, 3, 2
  %83 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %82, i64 %1, 3, 3
  %84 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %83, i64 %22, 4, 0
  %85 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %84, i64 %21, 4, 1
  %86 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %85, i64 %1, 4, 2
  %87 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %86, i64 1, 4, 3
  %88 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %17, 0
  %89 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %17, 1
  %90 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %88, 0
  %91 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %90, ptr %89, 1
  %92 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %91, i64 0, 2
  %93 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %92, i64 %0, 3, 0
  %94 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %93, i64 %1, 3, 1
  %95 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %94, i64 %2, 3, 2
  %96 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %95, i64 %3, 3, 3
  %97 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %96, i64 %20, 4, 0
  %98 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %97, i64 %19, 4, 1
  %99 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %98, i64 %3, 4, 2
  %100 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %99, i64 1, 4, 3
  %101 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %18, 0
  %102 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %18, 1
  %103 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %101, 0
  %104 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %103, ptr %102, 1
  %105 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %104, i64 0, 2
  %106 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %105, i64 %0, 3, 0
  %107 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %106, i64 %1, 3, 1
  %108 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %107, i64 %2, 3, 2
  %109 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %108, i64 %3, 3, 3
  %110 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %109, i64 %20, 4, 0
  %111 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %110, i64 %19, 4, 1
  %112 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %111, i64 %3, 4, 2
  %113 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %112, i64 1, 4, 3
  %114 = call float @bench_inv_sqrt_index(i64 %3)
  br label %115

115:                                              ; preds = %124, %11
  %116 = phi i64 [ %125, %124 ], [ 0, %11 ]
  %117 = icmp slt i64 %116, %0
  br i1 %117, label %118, label %119

118:                                              ; preds = %115
  br label %120

119:                                              ; preds = %115
  br label %190

120:                                              ; preds = %130, %118
  %121 = phi i64 [ %131, %130 ], [ 0, %118 ]
  %122 = icmp slt i64 %121, %2
  br i1 %122, label %123, label %124

123:                                              ; preds = %120
  br label %126

124:                                              ; preds = %120
  %125 = add i64 %116, 1
  br label %115

126:                                              ; preds = %136, %123
  %127 = phi i64 [ %137, %136 ], [ 0, %123 ]
  %128 = icmp slt i64 %127, %1
  br i1 %128, label %129, label %130

129:                                              ; preds = %126
  br label %132

130:                                              ; preds = %126
  %131 = add i64 %121, 1
  br label %120

132:                                              ; preds = %174, %129
  %133 = phi i64 [ %189, %174 ], [ 0, %129 ]
  %134 = icmp slt i64 %133, %1
  br i1 %134, label %135, label %136

135:                                              ; preds = %132
  br label %138

136:                                              ; preds = %132
  %137 = add i64 %127, 1
  br label %126

138:                                              ; preds = %142, %135
  %139 = phi i64 [ %173, %142 ], [ 0, %135 ]
  %140 = phi float [ %172, %142 ], [ 0.000000e+00, %135 ]
  %141 = icmp slt i64 %139, %3
  br i1 %141, label %142, label %174

142:                                              ; preds = %138
  %143 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %35, 1
  %144 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %35, 4, 0
  %145 = mul i64 %116, %144
  %146 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %35, 4, 1
  %147 = mul i64 %127, %146
  %148 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %35, 4, 2
  %149 = mul i64 %121, %148
  %150 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %35, 4, 3
  %151 = mul i64 %139, %150
  %152 = add i64 %145, %147
  %153 = add i64 %152, %149
  %154 = add i64 %153, %151
  %155 = getelementptr float, ptr %143, i64 %154
  %156 = load float, ptr %155, align 4
  %157 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %48, 1
  %158 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %48, 4, 0
  %159 = mul i64 %116, %158
  %160 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %48, 4, 1
  %161 = mul i64 %133, %160
  %162 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %48, 4, 2
  %163 = mul i64 %121, %162
  %164 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %48, 4, 3
  %165 = mul i64 %139, %164
  %166 = add i64 %159, %161
  %167 = add i64 %166, %163
  %168 = add i64 %167, %165
  %169 = getelementptr float, ptr %157, i64 %168
  %170 = load float, ptr %169, align 4
  %171 = fmul float %156, %170
  %172 = fadd float %140, %171
  %173 = add i64 %139, 1
  br label %138

174:                                              ; preds = %138
  %175 = fmul float %140, %114
  %176 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 1
  %177 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 0
  %178 = mul i64 %116, %177
  %179 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 1
  %180 = mul i64 %121, %179
  %181 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 2
  %182 = mul i64 %127, %181
  %183 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 3
  %184 = mul i64 %133, %183
  %185 = add i64 %178, %180
  %186 = add i64 %185, %182
  %187 = add i64 %186, %184
  %188 = getelementptr float, ptr %176, i64 %187
  store float %175, ptr %188, align 4
  %189 = add i64 %133, 1
  br label %132

190:                                              ; preds = %199, %119
  %191 = phi i64 [ %200, %199 ], [ 0, %119 ]
  %192 = icmp slt i64 %191, %0
  br i1 %192, label %193, label %194

193:                                              ; preds = %190
  br label %195

194:                                              ; preds = %190
  br label %301

195:                                              ; preds = %205, %193
  %196 = phi i64 [ %206, %205 ], [ 0, %193 ]
  %197 = icmp slt i64 %196, %2
  br i1 %197, label %198, label %199

198:                                              ; preds = %195
  br label %201

199:                                              ; preds = %195
  %200 = add i64 %191, 1
  br label %190

201:                                              ; preds = %299, %198
  %202 = phi i64 [ %300, %299 ], [ 0, %198 ]
  %203 = icmp slt i64 %202, %1
  br i1 %203, label %204, label %205

204:                                              ; preds = %201
  br label %207

205:                                              ; preds = %201
  %206 = add i64 %196, 1
  br label %195

207:                                              ; preds = %211, %204
  %208 = phi i64 [ %227, %211 ], [ 0, %204 ]
  %209 = phi float [ %226, %211 ], [ 0xC7EFFFFFE0000000, %204 ]
  %210 = icmp slt i64 %208, %1
  br i1 %210, label %211, label %228

211:                                              ; preds = %207
  %212 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 1
  %213 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 0
  %214 = mul i64 %191, %213
  %215 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 1
  %216 = mul i64 %196, %215
  %217 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 2
  %218 = mul i64 %202, %217
  %219 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 3
  %220 = mul i64 %208, %219
  %221 = add i64 %214, %216
  %222 = add i64 %221, %218
  %223 = add i64 %222, %220
  %224 = getelementptr float, ptr %212, i64 %223
  %225 = load float, ptr %224, align 4
  %226 = call float @llvm.maximum.f32(float %209, float %225)
  %227 = add i64 %208, 1
  br label %207

228:                                              ; preds = %207
  br label %229

229:                                              ; preds = %233, %228
  %230 = phi i64 [ %264, %233 ], [ 0, %228 ]
  %231 = phi float [ %263, %233 ], [ 0.000000e+00, %228 ]
  %232 = icmp slt i64 %230, %1
  br i1 %232, label %233, label %265

233:                                              ; preds = %229
  %234 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 1
  %235 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 0
  %236 = mul i64 %191, %235
  %237 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 1
  %238 = mul i64 %196, %237
  %239 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 2
  %240 = mul i64 %202, %239
  %241 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %74, 4, 3
  %242 = mul i64 %230, %241
  %243 = add i64 %236, %238
  %244 = add i64 %243, %240
  %245 = add i64 %244, %242
  %246 = getelementptr float, ptr %234, i64 %245
  %247 = load float, ptr %246, align 4
  %248 = fsub float %247, %209
  %249 = call float @bench_expf(float %248)
  %250 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 1
  %251 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 0
  %252 = mul i64 %191, %251
  %253 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 1
  %254 = mul i64 %196, %253
  %255 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 2
  %256 = mul i64 %202, %255
  %257 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 3
  %258 = mul i64 %230, %257
  %259 = add i64 %252, %254
  %260 = add i64 %259, %256
  %261 = add i64 %260, %258
  %262 = getelementptr float, ptr %250, i64 %261
  store float %249, ptr %262, align 4
  %263 = fadd float %231, %249
  %264 = add i64 %230, 1
  br label %229

265:                                              ; preds = %229
  br label %266

266:                                              ; preds = %269, %265
  %267 = phi i64 [ %298, %269 ], [ 0, %265 ]
  %268 = icmp slt i64 %267, %1
  br i1 %268, label %269, label %299

269:                                              ; preds = %266
  %270 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 1
  %271 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 0
  %272 = mul i64 %191, %271
  %273 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 1
  %274 = mul i64 %196, %273
  %275 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 2
  %276 = mul i64 %202, %275
  %277 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 3
  %278 = mul i64 %267, %277
  %279 = add i64 %272, %274
  %280 = add i64 %279, %276
  %281 = add i64 %280, %278
  %282 = getelementptr float, ptr %270, i64 %281
  %283 = load float, ptr %282, align 4
  %284 = fdiv float %283, %231
  %285 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 1
  %286 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 0
  %287 = mul i64 %191, %286
  %288 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 1
  %289 = mul i64 %196, %288
  %290 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 2
  %291 = mul i64 %202, %290
  %292 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 3
  %293 = mul i64 %267, %292
  %294 = add i64 %287, %289
  %295 = add i64 %294, %291
  %296 = add i64 %295, %293
  %297 = getelementptr float, ptr %285, i64 %296
  store float %284, ptr %297, align 4
  %298 = add i64 %267, 1
  br label %266

299:                                              ; preds = %266
  %300 = add i64 %202, 1
  br label %201

301:                                              ; preds = %310, %194
  %302 = phi i64 [ %311, %310 ], [ 0, %194 ]
  %303 = icmp slt i64 %302, %0
  br i1 %303, label %304, label %305

304:                                              ; preds = %301
  br label %306

305:                                              ; preds = %301
  ret void

306:                                              ; preds = %378, %304
  %307 = phi i64 [ %379, %378 ], [ 0, %304 ]
  %308 = icmp slt i64 %307, %1
  br i1 %308, label %309, label %310

309:                                              ; preds = %306
  br label %312

310:                                              ; preds = %306
  %311 = add i64 %302, 1
  br label %301

312:                                              ; preds = %321, %309
  %313 = phi i64 [ %322, %321 ], [ 0, %309 ]
  %314 = icmp slt i64 %313, %2
  br i1 %314, label %315, label %316

315:                                              ; preds = %312
  br label %317

316:                                              ; preds = %312
  br label %374

317:                                              ; preds = %359, %315
  %318 = phi i64 [ %373, %359 ], [ 0, %315 ]
  %319 = icmp slt i64 %318, %3
  br i1 %319, label %320, label %321

320:                                              ; preds = %317
  br label %323

321:                                              ; preds = %317
  %322 = add i64 %313, 1
  br label %312

323:                                              ; preds = %327, %320
  %324 = phi i64 [ %358, %327 ], [ 0, %320 ]
  %325 = phi float [ %357, %327 ], [ 0.000000e+00, %320 ]
  %326 = icmp slt i64 %324, %1
  br i1 %326, label %327, label %359

327:                                              ; preds = %323
  %328 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 1
  %329 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 0
  %330 = mul i64 %302, %329
  %331 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 1
  %332 = mul i64 %313, %331
  %333 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 2
  %334 = mul i64 %307, %333
  %335 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %87, 4, 3
  %336 = mul i64 %324, %335
  %337 = add i64 %330, %332
  %338 = add i64 %337, %334
  %339 = add i64 %338, %336
  %340 = getelementptr float, ptr %328, i64 %339
  %341 = load float, ptr %340, align 4
  %342 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %61, 1
  %343 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %61, 4, 0
  %344 = mul i64 %302, %343
  %345 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %61, 4, 1
  %346 = mul i64 %324, %345
  %347 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %61, 4, 2
  %348 = mul i64 %313, %347
  %349 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %61, 4, 3
  %350 = mul i64 %318, %349
  %351 = add i64 %344, %346
  %352 = add i64 %351, %348
  %353 = add i64 %352, %350
  %354 = getelementptr float, ptr %342, i64 %353
  %355 = load float, ptr %354, align 4
  %356 = fmul float %341, %355
  %357 = fadd float %325, %356
  %358 = add i64 %324, 1
  br label %323

359:                                              ; preds = %323
  %360 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 1
  %361 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 4, 0
  %362 = mul i64 %302, %361
  %363 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 4, 1
  %364 = mul i64 %307, %363
  %365 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 4, 2
  %366 = mul i64 %313, %365
  %367 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 4, 3
  %368 = mul i64 %318, %367
  %369 = add i64 %362, %364
  %370 = add i64 %369, %366
  %371 = add i64 %370, %368
  %372 = getelementptr float, ptr %360, i64 %371
  store float %325, ptr %372, align 4
  %373 = add i64 %318, 1
  br label %317

374:                                              ; preds = %412, %316
  %375 = phi i64 [ %413, %412 ], [ 0, %316 ]
  %376 = icmp slt i64 %375, %2
  br i1 %376, label %377, label %378

377:                                              ; preds = %374
  br label %380

378:                                              ; preds = %374
  %379 = add i64 %307, 1
  br label %306

380:                                              ; preds = %383, %377
  %381 = phi i64 [ %411, %383 ], [ 0, %377 ]
  %382 = icmp slt i64 %381, %3
  br i1 %382, label %383, label %412

383:                                              ; preds = %380
  %384 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 1
  %385 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 4, 0
  %386 = mul i64 %302, %385
  %387 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 4, 1
  %388 = mul i64 %307, %387
  %389 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 4, 2
  %390 = mul i64 %375, %389
  %391 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %100, 4, 3
  %392 = mul i64 %381, %391
  %393 = add i64 %386, %388
  %394 = add i64 %393, %390
  %395 = add i64 %394, %392
  %396 = getelementptr float, ptr %384, i64 %395
  %397 = load float, ptr %396, align 4
  %398 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %113, 1
  %399 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %113, 4, 0
  %400 = mul i64 %302, %399
  %401 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %113, 4, 1
  %402 = mul i64 %307, %401
  %403 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %113, 4, 2
  %404 = mul i64 %375, %403
  %405 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %113, 4, 3
  %406 = mul i64 %381, %405
  %407 = add i64 %400, %402
  %408 = add i64 %407, %404
  %409 = add i64 %408, %406
  %410 = getelementptr float, ptr %398, i64 %409
  store float %397, ptr %410, align 4
  %411 = add i64 %381, 1
  br label %380

412:                                              ; preds = %380
  %413 = add i64 %375, 1
  br label %374
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.maximum.f32(float, float) #0

attributes #0 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

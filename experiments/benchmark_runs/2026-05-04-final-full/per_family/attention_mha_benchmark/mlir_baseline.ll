; ModuleID = 'LLVMDialectModule'
source_filename = "LLVMDialectModule"

declare float @bench_expf(float)

declare float @bench_inv_sqrt_index(i64)

define void @attention_mha(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5, i64 %6, i64 %7, i64 %8, ptr %9, ptr %10, i64 %11, i64 %12, i64 %13, ptr %14, ptr %15, i64 %16, i64 %17, i64 %18, ptr %19, ptr %20, i64 %21, i64 %22, i64 %23, ptr %24, ptr %25, i64 %26, i64 %27, i64 %28, ptr %29, ptr %30, i64 %31, i64 %32, i64 %33, ptr %34, ptr %35, i64 %36, i64 %37, i64 %38) {
  %40 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %34, 0
  %41 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %40, ptr %35, 1
  %42 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %41, i64 %36, 2
  %43 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %42, i64 %37, 3, 0
  %44 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %43, i64 %38, 4, 0
  %45 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %29, 0
  %46 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %45, ptr %30, 1
  %47 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %46, i64 %31, 2
  %48 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %47, i64 %32, 3, 0
  %49 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %48, i64 %33, 4, 0
  %50 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %24, 0
  %51 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %50, ptr %25, 1
  %52 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %51, i64 %26, 2
  %53 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %52, i64 %27, 3, 0
  %54 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %53, i64 %28, 4, 0
  %55 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %19, 0
  %56 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %55, ptr %20, 1
  %57 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %56, i64 %21, 2
  %58 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %57, i64 %22, 3, 0
  %59 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %58, i64 %23, 4, 0
  %60 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %14, 0
  %61 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %60, ptr %15, 1
  %62 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %61, i64 %16, 2
  %63 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %62, i64 %17, 3, 0
  %64 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %63, i64 %18, 4, 0
  %65 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %9, 0
  %66 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %65, ptr %10, 1
  %67 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %66, i64 %11, 2
  %68 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %67, i64 %12, 3, 0
  %69 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %68, i64 %13, 4, 0
  %70 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } poison, ptr %4, 0
  %71 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %70, ptr %5, 1
  %72 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %71, i64 %6, 2
  %73 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %72, i64 %7, 3, 0
  %74 = insertvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %73, i64 %8, 4, 0
  %75 = mul i64 %2, %3
  %76 = mul i64 %1, %75
  %77 = mul i64 %1, %1
  %78 = mul i64 %2, %77
  %79 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %74, 0
  %80 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %74, 1
  %81 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } poison, ptr %79, 0
  %82 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %81, ptr %80, 1
  %83 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %82, i64 0, 2
  %84 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %83, i64 %0, 3, 0
  %85 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %84, i64 %76, 4, 0
  %86 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %85, i64 %1, 3, 1
  %87 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %86, i64 %75, 4, 1
  %88 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %87, i64 %75, 3, 2
  %89 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %88, i64 1, 4, 2
  %90 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %69, 0
  %91 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %69, 1
  %92 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } poison, ptr %90, 0
  %93 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %92, ptr %91, 1
  %94 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %93, i64 0, 2
  %95 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %94, i64 %0, 3, 0
  %96 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %95, i64 %76, 4, 0
  %97 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %96, i64 %1, 3, 1
  %98 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %97, i64 %75, 4, 1
  %99 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %98, i64 %75, 3, 2
  %100 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %99, i64 1, 4, 2
  %101 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %64, 0
  %102 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %64, 1
  %103 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } poison, ptr %101, 0
  %104 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %103, ptr %102, 1
  %105 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %104, i64 0, 2
  %106 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %105, i64 %0, 3, 0
  %107 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %106, i64 %76, 4, 0
  %108 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %107, i64 %1, 3, 1
  %109 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %108, i64 %75, 4, 1
  %110 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %109, i64 %75, 3, 2
  %111 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %110, i64 1, 4, 2
  %112 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %59, 0
  %113 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %59, 1
  %114 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %112, 0
  %115 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %114, ptr %113, 1
  %116 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %115, i64 0, 2
  %117 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %116, i64 %0, 3, 0
  %118 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %117, i64 %78, 4, 0
  %119 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %118, i64 %2, 3, 1
  %120 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %119, i64 %77, 4, 1
  %121 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %120, i64 %1, 3, 2
  %122 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %121, i64 %1, 4, 2
  %123 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %122, i64 %1, 3, 3
  %124 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %123, i64 1, 4, 3
  %125 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %54, 0
  %126 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %54, 1
  %127 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } poison, ptr %125, 0
  %128 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %127, ptr %126, 1
  %129 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %128, i64 0, 2
  %130 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %129, i64 %0, 3, 0
  %131 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %130, i64 %78, 4, 0
  %132 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %131, i64 %2, 3, 1
  %133 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %132, i64 %77, 4, 1
  %134 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %133, i64 %1, 3, 2
  %135 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %134, i64 %1, 4, 2
  %136 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %135, i64 %1, 3, 3
  %137 = insertvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %136, i64 1, 4, 3
  %138 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %44, 0
  %139 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %44, 1
  %140 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } poison, ptr %138, 0
  %141 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %140, ptr %139, 1
  %142 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %141, i64 0, 2
  %143 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %142, i64 %0, 3, 0
  %144 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %143, i64 %76, 4, 0
  %145 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %144, i64 %1, 3, 1
  %146 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %145, i64 %75, 4, 1
  %147 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %146, i64 %75, 3, 2
  %148 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %147, i64 1, 4, 2
  %149 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %49, 0
  %150 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %49, 1
  %151 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } poison, ptr %149, 0
  %152 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %151, ptr %150, 1
  %153 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %152, i64 0, 2
  %154 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %153, i64 %0, 3, 0
  %155 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %154, i64 %76, 4, 0
  %156 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %155, i64 %1, 3, 1
  %157 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %156, i64 %75, 4, 1
  %158 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %157, i64 %75, 3, 2
  %159 = insertvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %158, i64 1, 4, 2
  %160 = call float @bench_inv_sqrt_index(i64 %3)
  br label %161

161:                                              ; preds = %251, %39
  %162 = phi i64 [ %252, %251 ], [ 0, %39 ]
  %163 = icmp slt i64 %162, %0
  br i1 %163, label %164, label %253

164:                                              ; preds = %161
  br label %165

165:                                              ; preds = %249, %164
  %166 = phi i64 [ %250, %249 ], [ 0, %164 ]
  %167 = icmp slt i64 %166, %2
  br i1 %167, label %168, label %251

168:                                              ; preds = %165
  %169 = add i64 %162, 32
  %170 = call i64 @llvm.smin.i64(i64 %169, i64 %0)
  br label %171

171:                                              ; preds = %247, %168
  %172 = phi i64 [ %248, %247 ], [ %162, %168 ]
  %173 = icmp slt i64 %172, %170
  br i1 %173, label %174, label %249

174:                                              ; preds = %171
  %175 = add i64 %166, 32
  %176 = call i64 @llvm.smin.i64(i64 %175, i64 %2)
  br label %177

177:                                              ; preds = %245, %174
  %178 = phi i64 [ %246, %245 ], [ %166, %174 ]
  %179 = icmp slt i64 %178, %176
  br i1 %179, label %180, label %247

180:                                              ; preds = %177
  %181 = mul i64 %178, %3
  br label %182

182:                                              ; preds = %243, %180
  %183 = phi i64 [ %244, %243 ], [ 0, %180 ]
  %184 = icmp slt i64 %183, %1
  br i1 %184, label %185, label %245

185:                                              ; preds = %182
  br label %186

186:                                              ; preds = %225, %185
  %187 = phi i64 [ %242, %225 ], [ 0, %185 ]
  %188 = icmp slt i64 %187, %1
  br i1 %188, label %189, label %243

189:                                              ; preds = %186
  br label %190

190:                                              ; preds = %194, %189
  %191 = phi i64 [ %224, %194 ], [ 0, %189 ]
  %192 = phi float [ %223, %194 ], [ 0.000000e+00, %189 ]
  %193 = icmp slt i64 %191, %3
  br i1 %193, label %194, label %225

194:                                              ; preds = %190
  %195 = add i64 %181, %191
  %196 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %89, 1
  %197 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %89, 2
  %198 = getelementptr float, ptr %196, i64 %197
  %199 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %89, 4, 0
  %200 = mul nuw nsw i64 %172, %199
  %201 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %89, 4, 1
  %202 = mul nuw nsw i64 %183, %201
  %203 = add nuw nsw i64 %200, %202
  %204 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %89, 4, 2
  %205 = mul nuw nsw i64 %195, %204
  %206 = add nuw nsw i64 %203, %205
  %207 = getelementptr inbounds nuw float, ptr %198, i64 %206
  %208 = load float, ptr %207, align 4
  %209 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %100, 1
  %210 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %100, 2
  %211 = getelementptr float, ptr %209, i64 %210
  %212 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %100, 4, 0
  %213 = mul nuw nsw i64 %172, %212
  %214 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %100, 4, 1
  %215 = mul nuw nsw i64 %187, %214
  %216 = add nuw nsw i64 %213, %215
  %217 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %100, 4, 2
  %218 = mul nuw nsw i64 %195, %217
  %219 = add nuw nsw i64 %216, %218
  %220 = getelementptr inbounds nuw float, ptr %211, i64 %219
  %221 = load float, ptr %220, align 4
  %222 = fmul float %208, %221
  %223 = fadd float %192, %222
  %224 = add i64 %191, 1
  br label %190

225:                                              ; preds = %190
  %226 = fmul float %192, %160
  %227 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 1
  %228 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 2
  %229 = getelementptr float, ptr %227, i64 %228
  %230 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 0
  %231 = mul nuw nsw i64 %172, %230
  %232 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 1
  %233 = mul nuw nsw i64 %178, %232
  %234 = add nuw nsw i64 %231, %233
  %235 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 2
  %236 = mul nuw nsw i64 %183, %235
  %237 = add nuw nsw i64 %234, %236
  %238 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 3
  %239 = mul nuw nsw i64 %187, %238
  %240 = add nuw nsw i64 %237, %239
  %241 = getelementptr inbounds nuw float, ptr %229, i64 %240
  store float %226, ptr %241, align 4
  %242 = add i64 %187, 1
  br label %186

243:                                              ; preds = %186
  %244 = add i64 %183, 1
  br label %182

245:                                              ; preds = %182
  %246 = add i64 %178, 1
  br label %177

247:                                              ; preds = %177
  %248 = add i64 %172, 1
  br label %171

249:                                              ; preds = %171
  %250 = add i64 %166, 32
  br label %165

251:                                              ; preds = %165
  %252 = add i64 %162, 32
  br label %161

253:                                              ; preds = %161
  br label %254

254:                                              ; preds = %396, %253
  %255 = phi i64 [ %397, %396 ], [ 0, %253 ]
  %256 = icmp slt i64 %255, %0
  br i1 %256, label %257, label %398

257:                                              ; preds = %254
  br label %258

258:                                              ; preds = %394, %257
  %259 = phi i64 [ %395, %394 ], [ 0, %257 ]
  %260 = icmp slt i64 %259, %2
  br i1 %260, label %261, label %396

261:                                              ; preds = %258
  br label %262

262:                                              ; preds = %392, %261
  %263 = phi i64 [ %393, %392 ], [ 0, %261 ]
  %264 = icmp slt i64 %263, %1
  br i1 %264, label %265, label %394

265:                                              ; preds = %262
  %266 = add i64 %255, 32
  %267 = call i64 @llvm.smin.i64(i64 %266, i64 %0)
  br label %268

268:                                              ; preds = %390, %265
  %269 = phi i64 [ %391, %390 ], [ %255, %265 ]
  %270 = icmp slt i64 %269, %267
  br i1 %270, label %271, label %392

271:                                              ; preds = %268
  %272 = add i64 %259, 32
  %273 = call i64 @llvm.smin.i64(i64 %272, i64 %2)
  br label %274

274:                                              ; preds = %388, %271
  %275 = phi i64 [ %389, %388 ], [ %259, %271 ]
  %276 = icmp slt i64 %275, %273
  br i1 %276, label %277, label %390

277:                                              ; preds = %274
  %278 = add i64 %263, 32
  %279 = call i64 @llvm.smin.i64(i64 %278, i64 %1)
  br label %280

280:                                              ; preds = %386, %277
  %281 = phi i64 [ %387, %386 ], [ %263, %277 ]
  %282 = icmp slt i64 %281, %279
  br i1 %282, label %283, label %388

283:                                              ; preds = %280
  br label %284

284:                                              ; preds = %288, %283
  %285 = phi i64 [ %306, %288 ], [ 0, %283 ]
  %286 = phi float [ %305, %288 ], [ 0xC7EFFFFFE0000000, %283 ]
  %287 = icmp slt i64 %285, %1
  br i1 %287, label %288, label %307

288:                                              ; preds = %284
  %289 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 1
  %290 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 2
  %291 = getelementptr float, ptr %289, i64 %290
  %292 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 0
  %293 = mul nuw nsw i64 %269, %292
  %294 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 1
  %295 = mul nuw nsw i64 %275, %294
  %296 = add nuw nsw i64 %293, %295
  %297 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 2
  %298 = mul nuw nsw i64 %281, %297
  %299 = add nuw nsw i64 %296, %298
  %300 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 3
  %301 = mul nuw nsw i64 %285, %300
  %302 = add nuw nsw i64 %299, %301
  %303 = getelementptr inbounds nuw float, ptr %291, i64 %302
  %304 = load float, ptr %303, align 4
  %305 = call float @llvm.maximum.f32(float %286, float %304)
  %306 = add i64 %285, 1
  br label %284

307:                                              ; preds = %284
  br label %308

308:                                              ; preds = %312, %307
  %309 = phi i64 [ %347, %312 ], [ 0, %307 ]
  %310 = phi float [ %346, %312 ], [ 0.000000e+00, %307 ]
  %311 = icmp slt i64 %309, %1
  br i1 %311, label %312, label %348

312:                                              ; preds = %308
  %313 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 1
  %314 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 2
  %315 = getelementptr float, ptr %313, i64 %314
  %316 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 0
  %317 = mul nuw nsw i64 %269, %316
  %318 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 1
  %319 = mul nuw nsw i64 %275, %318
  %320 = add nuw nsw i64 %317, %319
  %321 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 2
  %322 = mul nuw nsw i64 %281, %321
  %323 = add nuw nsw i64 %320, %322
  %324 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %124, 4, 3
  %325 = mul nuw nsw i64 %309, %324
  %326 = add nuw nsw i64 %323, %325
  %327 = getelementptr inbounds nuw float, ptr %315, i64 %326
  %328 = load float, ptr %327, align 4
  %329 = fsub float %328, %286
  %330 = call float @bench_expf(float %329)
  %331 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 1
  %332 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 2
  %333 = getelementptr float, ptr %331, i64 %332
  %334 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 0
  %335 = mul nuw nsw i64 %269, %334
  %336 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 1
  %337 = mul nuw nsw i64 %275, %336
  %338 = add nuw nsw i64 %335, %337
  %339 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 2
  %340 = mul nuw nsw i64 %281, %339
  %341 = add nuw nsw i64 %338, %340
  %342 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 3
  %343 = mul nuw nsw i64 %309, %342
  %344 = add nuw nsw i64 %341, %343
  %345 = getelementptr inbounds nuw float, ptr %333, i64 %344
  store float %330, ptr %345, align 4
  %346 = fadd float %310, %330
  %347 = add i64 %309, 1
  br label %308

348:                                              ; preds = %308
  br label %349

349:                                              ; preds = %352, %348
  %350 = phi i64 [ %385, %352 ], [ 0, %348 ]
  %351 = icmp slt i64 %350, %1
  br i1 %351, label %352, label %386

352:                                              ; preds = %349
  %353 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 1
  %354 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 2
  %355 = getelementptr float, ptr %353, i64 %354
  %356 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 0
  %357 = mul nuw nsw i64 %269, %356
  %358 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 1
  %359 = mul nuw nsw i64 %275, %358
  %360 = add nuw nsw i64 %357, %359
  %361 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 2
  %362 = mul nuw nsw i64 %281, %361
  %363 = add nuw nsw i64 %360, %362
  %364 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 3
  %365 = mul nuw nsw i64 %350, %364
  %366 = add nuw nsw i64 %363, %365
  %367 = getelementptr inbounds nuw float, ptr %355, i64 %366
  %368 = load float, ptr %367, align 4
  %369 = fdiv float %368, %310
  %370 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 1
  %371 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 2
  %372 = getelementptr float, ptr %370, i64 %371
  %373 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 0
  %374 = mul nuw nsw i64 %269, %373
  %375 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 1
  %376 = mul nuw nsw i64 %275, %375
  %377 = add nuw nsw i64 %374, %376
  %378 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 2
  %379 = mul nuw nsw i64 %281, %378
  %380 = add nuw nsw i64 %377, %379
  %381 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 3
  %382 = mul nuw nsw i64 %350, %381
  %383 = add nuw nsw i64 %380, %382
  %384 = getelementptr inbounds nuw float, ptr %372, i64 %383
  store float %369, ptr %384, align 4
  %385 = add i64 %350, 1
  br label %349

386:                                              ; preds = %349
  %387 = add i64 %281, 1
  br label %280

388:                                              ; preds = %280
  %389 = add i64 %275, 1
  br label %274

390:                                              ; preds = %274
  %391 = add i64 %269, 1
  br label %268

392:                                              ; preds = %268
  %393 = add i64 %263, 32
  br label %262

394:                                              ; preds = %262
  %395 = add i64 %259, 32
  br label %258

396:                                              ; preds = %258
  %397 = add i64 %255, 32
  br label %254

398:                                              ; preds = %254
  br label %399

399:                                              ; preds = %496, %398
  %400 = phi i64 [ %497, %496 ], [ 0, %398 ]
  %401 = icmp slt i64 %400, %0
  br i1 %401, label %402, label %498

402:                                              ; preds = %399
  br label %403

403:                                              ; preds = %494, %402
  %404 = phi i64 [ %495, %494 ], [ 0, %402 ]
  %405 = icmp slt i64 %404, %1
  br i1 %405, label %406, label %496

406:                                              ; preds = %403
  br label %407

407:                                              ; preds = %492, %406
  %408 = phi i64 [ %493, %492 ], [ 0, %406 ]
  %409 = icmp slt i64 %408, %2
  br i1 %409, label %410, label %494

410:                                              ; preds = %407
  %411 = add i64 %400, 32
  %412 = call i64 @llvm.smin.i64(i64 %411, i64 %0)
  br label %413

413:                                              ; preds = %490, %410
  %414 = phi i64 [ %491, %490 ], [ %400, %410 ]
  %415 = icmp slt i64 %414, %412
  br i1 %415, label %416, label %492

416:                                              ; preds = %413
  %417 = add i64 %404, 32
  %418 = call i64 @llvm.smin.i64(i64 %417, i64 %1)
  br label %419

419:                                              ; preds = %488, %416
  %420 = phi i64 [ %489, %488 ], [ %404, %416 ]
  %421 = icmp slt i64 %420, %418
  br i1 %421, label %422, label %490

422:                                              ; preds = %419
  %423 = add i64 %408, 32
  %424 = call i64 @llvm.smin.i64(i64 %423, i64 %2)
  br label %425

425:                                              ; preds = %486, %422
  %426 = phi i64 [ %487, %486 ], [ %408, %422 ]
  %427 = icmp slt i64 %426, %424
  br i1 %427, label %428, label %488

428:                                              ; preds = %425
  %429 = mul i64 %426, %3
  br label %430

430:                                              ; preds = %472, %428
  %431 = phi i64 [ %485, %472 ], [ 0, %428 ]
  %432 = icmp slt i64 %431, %3
  br i1 %432, label %433, label %486

433:                                              ; preds = %430
  %434 = add i64 %429, %431
  br label %435

435:                                              ; preds = %439, %433
  %436 = phi i64 [ %471, %439 ], [ 0, %433 ]
  %437 = phi float [ %470, %439 ], [ 0.000000e+00, %433 ]
  %438 = icmp slt i64 %436, %1
  br i1 %438, label %439, label %472

439:                                              ; preds = %435
  %440 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 1
  %441 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 2
  %442 = getelementptr float, ptr %440, i64 %441
  %443 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 0
  %444 = mul nuw nsw i64 %414, %443
  %445 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 1
  %446 = mul nuw nsw i64 %426, %445
  %447 = add nuw nsw i64 %444, %446
  %448 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 2
  %449 = mul nuw nsw i64 %420, %448
  %450 = add nuw nsw i64 %447, %449
  %451 = extractvalue { ptr, ptr, i64, [4 x i64], [4 x i64] } %137, 4, 3
  %452 = mul nuw nsw i64 %436, %451
  %453 = add nuw nsw i64 %450, %452
  %454 = getelementptr inbounds nuw float, ptr %442, i64 %453
  %455 = load float, ptr %454, align 4
  %456 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %111, 1
  %457 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %111, 2
  %458 = getelementptr float, ptr %456, i64 %457
  %459 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %111, 4, 0
  %460 = mul nuw nsw i64 %414, %459
  %461 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %111, 4, 1
  %462 = mul nuw nsw i64 %436, %461
  %463 = add nuw nsw i64 %460, %462
  %464 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %111, 4, 2
  %465 = mul nuw nsw i64 %434, %464
  %466 = add nuw nsw i64 %463, %465
  %467 = getelementptr inbounds nuw float, ptr %458, i64 %466
  %468 = load float, ptr %467, align 4
  %469 = fmul float %455, %468
  %470 = fadd float %437, %469
  %471 = add i64 %436, 1
  br label %435

472:                                              ; preds = %435
  %473 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 1
  %474 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 2
  %475 = getelementptr float, ptr %473, i64 %474
  %476 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 4, 0
  %477 = mul nuw nsw i64 %414, %476
  %478 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 4, 1
  %479 = mul nuw nsw i64 %420, %478
  %480 = add nuw nsw i64 %477, %479
  %481 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 4, 2
  %482 = mul nuw nsw i64 %434, %481
  %483 = add nuw nsw i64 %480, %482
  %484 = getelementptr inbounds nuw float, ptr %475, i64 %483
  store float %437, ptr %484, align 4
  %485 = add i64 %431, 1
  br label %430

486:                                              ; preds = %430
  %487 = add i64 %426, 1
  br label %425

488:                                              ; preds = %425
  %489 = add i64 %420, 1
  br label %419

490:                                              ; preds = %419
  %491 = add i64 %414, 1
  br label %413

492:                                              ; preds = %413
  %493 = add i64 %408, 32
  br label %407

494:                                              ; preds = %407
  %495 = add i64 %404, 32
  br label %403

496:                                              ; preds = %403
  %497 = add i64 %400, 32
  br label %399

498:                                              ; preds = %399
  br label %499

499:                                              ; preds = %539, %498
  %500 = phi i64 [ %540, %539 ], [ 0, %498 ]
  %501 = icmp slt i64 %500, %0
  br i1 %501, label %502, label %541

502:                                              ; preds = %499
  br label %503

503:                                              ; preds = %537, %502
  %504 = phi i64 [ %538, %537 ], [ 0, %502 ]
  %505 = icmp slt i64 %504, %1
  br i1 %505, label %506, label %539

506:                                              ; preds = %503
  br label %507

507:                                              ; preds = %510, %506
  %508 = phi i64 [ %536, %510 ], [ 0, %506 ]
  %509 = icmp slt i64 %508, %75
  br i1 %509, label %510, label %537

510:                                              ; preds = %507
  %511 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 1
  %512 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 2
  %513 = getelementptr float, ptr %511, i64 %512
  %514 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 4, 0
  %515 = mul nuw nsw i64 %500, %514
  %516 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 4, 1
  %517 = mul nuw nsw i64 %504, %516
  %518 = add nuw nsw i64 %515, %517
  %519 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %159, 4, 2
  %520 = mul nuw nsw i64 %508, %519
  %521 = add nuw nsw i64 %518, %520
  %522 = getelementptr inbounds nuw float, ptr %513, i64 %521
  %523 = load float, ptr %522, align 4
  %524 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %148, 1
  %525 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %148, 2
  %526 = getelementptr float, ptr %524, i64 %525
  %527 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %148, 4, 0
  %528 = mul nuw nsw i64 %500, %527
  %529 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %148, 4, 1
  %530 = mul nuw nsw i64 %504, %529
  %531 = add nuw nsw i64 %528, %530
  %532 = extractvalue { ptr, ptr, i64, [3 x i64], [3 x i64] } %148, 4, 2
  %533 = mul nuw nsw i64 %508, %532
  %534 = add nuw nsw i64 %531, %533
  %535 = getelementptr inbounds nuw float, ptr %526, i64 %534
  store float %523, ptr %535, align 4
  %536 = add i64 %508, 1
  br label %507

537:                                              ; preds = %507
  %538 = add i64 %504, 1
  br label %503

539:                                              ; preds = %503
  %540 = add i64 %500, 1
  br label %499

541:                                              ; preds = %499
  ret void
}

define void @_mlir_ciface_attention_mha(i64 %0, i64 %1, i64 %2, i64 %3, ptr %4, ptr %5, ptr %6, ptr %7, ptr %8, ptr %9, ptr %10) {
  %12 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %4, align 8
  %13 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %12, 0
  %14 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %12, 1
  %15 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %12, 2
  %16 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %12, 3, 0
  %17 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %12, 4, 0
  %18 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %5, align 8
  %19 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %18, 0
  %20 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %18, 1
  %21 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %18, 2
  %22 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %18, 3, 0
  %23 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %18, 4, 0
  %24 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %6, align 8
  %25 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, 0
  %26 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, 1
  %27 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, 2
  %28 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, 3, 0
  %29 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %24, 4, 0
  %30 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %7, align 8
  %31 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, 0
  %32 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, 1
  %33 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, 2
  %34 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, 3, 0
  %35 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %30, 4, 0
  %36 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %8, align 8
  %37 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %36, 0
  %38 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %36, 1
  %39 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %36, 2
  %40 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %36, 3, 0
  %41 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %36, 4, 0
  %42 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %9, align 8
  %43 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %42, 0
  %44 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %42, 1
  %45 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %42, 2
  %46 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %42, 3, 0
  %47 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %42, 4, 0
  %48 = load { ptr, ptr, i64, [1 x i64], [1 x i64] }, ptr %10, align 8
  %49 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %48, 0
  %50 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %48, 1
  %51 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %48, 2
  %52 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %48, 3, 0
  %53 = extractvalue { ptr, ptr, i64, [1 x i64], [1 x i64] } %48, 4, 0
  call void @attention_mha(i64 %0, i64 %1, i64 %2, i64 %3, ptr %13, ptr %14, i64 %15, i64 %16, i64 %17, ptr %19, ptr %20, i64 %21, i64 %22, i64 %23, ptr %25, ptr %26, i64 %27, i64 %28, i64 %29, ptr %31, ptr %32, i64 %33, i64 %34, i64 %35, ptr %37, ptr %38, i64 %39, i64 %40, i64 %41, ptr %43, ptr %44, i64 %45, i64 %46, i64 %47, ptr %49, ptr %50, i64 %51, i64 %52, i64 %53)
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.smin.i64(i64, i64) #0

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare float @llvm.maximum.f32(float, float) #0

attributes #0 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0}

!0 = !{i32 2, !"Debug Info Version", i32 3}

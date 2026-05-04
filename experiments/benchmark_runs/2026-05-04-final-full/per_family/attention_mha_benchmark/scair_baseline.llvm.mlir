module {
  llvm.func @bench_expf(f32) -> f32
  llvm.func @bench_inv_sqrt_index(i64) -> f32
  llvm.func @attention_mha(%arg0: i64, %arg1: i64, %arg2: i64, %arg3: i64, %arg4: !llvm.ptr, %arg5: !llvm.ptr, %arg6: !llvm.ptr, %arg7: !llvm.ptr, %arg8: !llvm.ptr, %arg9: !llvm.ptr, %arg10: !llvm.ptr) {
    %0 = llvm.load %arg4 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %1 = llvm.load %arg5 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %2 = llvm.load %arg6 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %3 = llvm.load %arg7 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %4 = llvm.load %arg8 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %5 = llvm.load %arg9 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %6 = llvm.load %arg10 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %7 = llvm.mlir.constant(0 : i64) : i64
    %8 = llvm.mlir.constant(1 : i64) : i64
    %9 = llvm.mlir.constant(0.000000e+00 : f32) : f32
    %10 = llvm.mlir.constant(-3.40282347E+38 : f32) : f32
    %11 = llvm.mul %arg2, %arg3 : i64
    %12 = llvm.mul %arg1, %11 : i64
    %13 = llvm.mul %arg1, %arg1 : i64
    %14 = llvm.mul %arg2, %13 : i64
    %15 = llvm.extractvalue %0[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %16 = llvm.extractvalue %0[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %17 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %18 = llvm.insertvalue %15, %17[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %19 = llvm.insertvalue %16, %18[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %20 = llvm.insertvalue %7, %19[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %21 = llvm.insertvalue %arg0, %20[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %22 = llvm.insertvalue %arg1, %21[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %23 = llvm.insertvalue %arg2, %22[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %24 = llvm.insertvalue %arg3, %23[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %25 = llvm.insertvalue %12, %24[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %26 = llvm.insertvalue %11, %25[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %27 = llvm.insertvalue %arg3, %26[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %28 = llvm.insertvalue %8, %27[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %29 = llvm.extractvalue %1[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %30 = llvm.extractvalue %1[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %31 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %32 = llvm.insertvalue %29, %31[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %33 = llvm.insertvalue %30, %32[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %34 = llvm.insertvalue %7, %33[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %35 = llvm.insertvalue %arg0, %34[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %36 = llvm.insertvalue %arg1, %35[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %37 = llvm.insertvalue %arg2, %36[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %38 = llvm.insertvalue %arg3, %37[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %39 = llvm.insertvalue %12, %38[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %40 = llvm.insertvalue %11, %39[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %41 = llvm.insertvalue %arg3, %40[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %42 = llvm.insertvalue %8, %41[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %43 = llvm.extractvalue %2[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %44 = llvm.extractvalue %2[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %45 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %46 = llvm.insertvalue %43, %45[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %47 = llvm.insertvalue %44, %46[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %48 = llvm.insertvalue %7, %47[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %49 = llvm.insertvalue %arg0, %48[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %50 = llvm.insertvalue %arg1, %49[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %51 = llvm.insertvalue %arg2, %50[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %52 = llvm.insertvalue %arg3, %51[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %53 = llvm.insertvalue %12, %52[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %54 = llvm.insertvalue %11, %53[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %55 = llvm.insertvalue %arg3, %54[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %56 = llvm.insertvalue %8, %55[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %57 = llvm.extractvalue %3[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %58 = llvm.extractvalue %3[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %59 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %60 = llvm.insertvalue %57, %59[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %61 = llvm.insertvalue %58, %60[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %62 = llvm.insertvalue %7, %61[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %63 = llvm.insertvalue %arg0, %62[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %64 = llvm.insertvalue %arg2, %63[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %65 = llvm.insertvalue %arg1, %64[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %66 = llvm.insertvalue %arg1, %65[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %67 = llvm.insertvalue %14, %66[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %68 = llvm.insertvalue %13, %67[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %69 = llvm.insertvalue %arg1, %68[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %70 = llvm.insertvalue %8, %69[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %71 = llvm.extractvalue %4[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %72 = llvm.extractvalue %4[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %73 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %74 = llvm.insertvalue %71, %73[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %75 = llvm.insertvalue %72, %74[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %76 = llvm.insertvalue %7, %75[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %77 = llvm.insertvalue %arg0, %76[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %78 = llvm.insertvalue %arg2, %77[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %79 = llvm.insertvalue %arg1, %78[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %80 = llvm.insertvalue %arg1, %79[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %81 = llvm.insertvalue %14, %80[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %82 = llvm.insertvalue %13, %81[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %83 = llvm.insertvalue %arg1, %82[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %84 = llvm.insertvalue %8, %83[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %85 = llvm.extractvalue %5[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %86 = llvm.extractvalue %5[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %87 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %88 = llvm.insertvalue %85, %87[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %89 = llvm.insertvalue %86, %88[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %90 = llvm.insertvalue %7, %89[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %91 = llvm.insertvalue %arg0, %90[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %92 = llvm.insertvalue %arg1, %91[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %93 = llvm.insertvalue %arg2, %92[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %94 = llvm.insertvalue %arg3, %93[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %95 = llvm.insertvalue %12, %94[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %96 = llvm.insertvalue %11, %95[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %97 = llvm.insertvalue %arg3, %96[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %98 = llvm.insertvalue %8, %97[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %99 = llvm.extractvalue %6[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %100 = llvm.extractvalue %6[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %101 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %102 = llvm.insertvalue %99, %101[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %103 = llvm.insertvalue %100, %102[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %104 = llvm.insertvalue %7, %103[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %105 = llvm.insertvalue %arg0, %104[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %106 = llvm.insertvalue %arg1, %105[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %107 = llvm.insertvalue %arg2, %106[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %108 = llvm.insertvalue %arg3, %107[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %109 = llvm.insertvalue %12, %108[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %110 = llvm.insertvalue %11, %109[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %111 = llvm.insertvalue %arg3, %110[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %112 = llvm.insertvalue %8, %111[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %113 = llvm.call @bench_inv_sqrt_index(%arg3) : (i64) -> f32
    llvm.br ^bb1(%7 : i64)
  ^bb1(%114: i64):  // 2 preds: ^bb0, ^bb6
    %115 = llvm.icmp "slt" %114, %arg0 : i64
    llvm.cond_br %115, ^bb2, ^bb3
  ^bb2:  // pred: ^bb1
    llvm.br ^bb4(%7 : i64)
  ^bb3:  // pred: ^bb1
    llvm.br ^bb16(%7 : i64)
  ^bb4(%116: i64):  // 2 preds: ^bb2, ^bb9
    %117 = llvm.icmp "slt" %116, %arg2 : i64
    llvm.cond_br %117, ^bb5, ^bb6
  ^bb5:  // pred: ^bb4
    llvm.br ^bb7(%7 : i64)
  ^bb6:  // pred: ^bb4
    %118 = llvm.mlir.constant(1 : i64) : i64
    %119 = llvm.add %114, %118 : i64
    llvm.br ^bb1(%119 : i64)
  ^bb7(%120: i64):  // 2 preds: ^bb5, ^bb12
    %121 = llvm.icmp "slt" %120, %arg1 : i64
    llvm.cond_br %121, ^bb8, ^bb9
  ^bb8:  // pred: ^bb7
    llvm.br ^bb10(%7 : i64)
  ^bb9:  // pred: ^bb7
    %122 = llvm.mlir.constant(1 : i64) : i64
    %123 = llvm.add %116, %122 : i64
    llvm.br ^bb4(%123 : i64)
  ^bb10(%124: i64):  // 2 preds: ^bb8, ^bb15
    %125 = llvm.icmp "slt" %124, %arg1 : i64
    llvm.cond_br %125, ^bb11, ^bb12
  ^bb11:  // pred: ^bb10
    llvm.br ^bb13(%7, %9 : i64, f32)
  ^bb12:  // pred: ^bb10
    %126 = llvm.mlir.constant(1 : i64) : i64
    %127 = llvm.add %120, %126 : i64
    llvm.br ^bb7(%127 : i64)
  ^bb13(%128: i64, %129: f32):  // 2 preds: ^bb11, ^bb14
    %130 = llvm.icmp "slt" %128, %arg3 : i64
    llvm.cond_br %130, ^bb14, ^bb15
  ^bb14:  // pred: ^bb13
    %131 = llvm.extractvalue %28[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %132 = llvm.extractvalue %28[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %133 = llvm.mul %114, %132 : i64
    %134 = llvm.extractvalue %28[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %135 = llvm.mul %120, %134 : i64
    %136 = llvm.extractvalue %28[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %137 = llvm.mul %116, %136 : i64
    %138 = llvm.extractvalue %28[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %139 = llvm.mul %128, %138 : i64
    %140 = llvm.add %133, %135 : i64
    %141 = llvm.add %140, %137 : i64
    %142 = llvm.add %141, %139 : i64
    %143 = llvm.getelementptr %131[%142] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %144 = llvm.load %143 : !llvm.ptr -> f32
    %145 = llvm.extractvalue %42[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %146 = llvm.extractvalue %42[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %147 = llvm.mul %114, %146 : i64
    %148 = llvm.extractvalue %42[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %149 = llvm.mul %124, %148 : i64
    %150 = llvm.extractvalue %42[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %151 = llvm.mul %116, %150 : i64
    %152 = llvm.extractvalue %42[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %153 = llvm.mul %128, %152 : i64
    %154 = llvm.add %147, %149 : i64
    %155 = llvm.add %154, %151 : i64
    %156 = llvm.add %155, %153 : i64
    %157 = llvm.getelementptr %145[%156] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %158 = llvm.load %157 : !llvm.ptr -> f32
    %159 = llvm.fmul %144, %158 : f32
    %160 = llvm.fadd %129, %159 : f32
    %161 = llvm.mlir.constant(1 : i64) : i64
    %162 = llvm.add %128, %161 : i64
    llvm.br ^bb13(%162, %160 : i64, f32)
  ^bb15:  // pred: ^bb13
    %163 = llvm.fmul %129, %113 : f32
    %164 = llvm.extractvalue %70[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %165 = llvm.extractvalue %70[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %166 = llvm.mul %114, %165 : i64
    %167 = llvm.extractvalue %70[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %168 = llvm.mul %116, %167 : i64
    %169 = llvm.extractvalue %70[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %170 = llvm.mul %120, %169 : i64
    %171 = llvm.extractvalue %70[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %172 = llvm.mul %124, %171 : i64
    %173 = llvm.add %166, %168 : i64
    %174 = llvm.add %173, %170 : i64
    %175 = llvm.add %174, %172 : i64
    %176 = llvm.getelementptr %164[%175] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %163, %176 : f32, !llvm.ptr
    %177 = llvm.mlir.constant(1 : i64) : i64
    %178 = llvm.add %124, %177 : i64
    llvm.br ^bb10(%178 : i64)
  ^bb16(%179: i64):  // 2 preds: ^bb3, ^bb21
    %180 = llvm.icmp "slt" %179, %arg0 : i64
    llvm.cond_br %180, ^bb17, ^bb18
  ^bb17:  // pred: ^bb16
    llvm.br ^bb19(%7 : i64)
  ^bb18:  // pred: ^bb16
    llvm.br ^bb34(%7 : i64)
  ^bb19(%181: i64):  // 2 preds: ^bb17, ^bb24
    %182 = llvm.icmp "slt" %181, %arg2 : i64
    llvm.cond_br %182, ^bb20, ^bb21
  ^bb20:  // pred: ^bb19
    llvm.br ^bb22(%7 : i64)
  ^bb21:  // pred: ^bb19
    %183 = llvm.mlir.constant(1 : i64) : i64
    %184 = llvm.add %179, %183 : i64
    llvm.br ^bb16(%184 : i64)
  ^bb22(%185: i64):  // 2 preds: ^bb20, ^bb33
    %186 = llvm.icmp "slt" %185, %arg1 : i64
    llvm.cond_br %186, ^bb23, ^bb24
  ^bb23:  // pred: ^bb22
    llvm.br ^bb25(%7, %10 : i64, f32)
  ^bb24:  // pred: ^bb22
    %187 = llvm.mlir.constant(1 : i64) : i64
    %188 = llvm.add %181, %187 : i64
    llvm.br ^bb19(%188 : i64)
  ^bb25(%189: i64, %190: f32):  // 2 preds: ^bb23, ^bb26
    %191 = llvm.icmp "slt" %189, %arg1 : i64
    llvm.cond_br %191, ^bb26, ^bb27
  ^bb26:  // pred: ^bb25
    %192 = llvm.extractvalue %70[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %193 = llvm.extractvalue %70[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %194 = llvm.mul %179, %193 : i64
    %195 = llvm.extractvalue %70[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %196 = llvm.mul %181, %195 : i64
    %197 = llvm.extractvalue %70[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %198 = llvm.mul %185, %197 : i64
    %199 = llvm.extractvalue %70[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %200 = llvm.mul %189, %199 : i64
    %201 = llvm.add %194, %196 : i64
    %202 = llvm.add %201, %198 : i64
    %203 = llvm.add %202, %200 : i64
    %204 = llvm.getelementptr %192[%203] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %205 = llvm.load %204 : !llvm.ptr -> f32
    %206 = llvm.intr.maximum(%190, %205) : (f32, f32) -> f32
    %207 = llvm.mlir.constant(1 : i64) : i64
    %208 = llvm.add %189, %207 : i64
    llvm.br ^bb25(%208, %206 : i64, f32)
  ^bb27:  // pred: ^bb25
    llvm.br ^bb28(%7, %9 : i64, f32)
  ^bb28(%209: i64, %210: f32):  // 2 preds: ^bb27, ^bb29
    %211 = llvm.icmp "slt" %209, %arg1 : i64
    llvm.cond_br %211, ^bb29, ^bb30
  ^bb29:  // pred: ^bb28
    %212 = llvm.extractvalue %70[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %213 = llvm.extractvalue %70[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %214 = llvm.mul %179, %213 : i64
    %215 = llvm.extractvalue %70[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %216 = llvm.mul %181, %215 : i64
    %217 = llvm.extractvalue %70[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %218 = llvm.mul %185, %217 : i64
    %219 = llvm.extractvalue %70[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %220 = llvm.mul %209, %219 : i64
    %221 = llvm.add %214, %216 : i64
    %222 = llvm.add %221, %218 : i64
    %223 = llvm.add %222, %220 : i64
    %224 = llvm.getelementptr %212[%223] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %225 = llvm.load %224 : !llvm.ptr -> f32
    %226 = llvm.fsub %225, %190 : f32
    %227 = llvm.call @bench_expf(%226) : (f32) -> f32
    %228 = llvm.extractvalue %84[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %229 = llvm.extractvalue %84[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %230 = llvm.mul %179, %229 : i64
    %231 = llvm.extractvalue %84[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %232 = llvm.mul %181, %231 : i64
    %233 = llvm.extractvalue %84[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %234 = llvm.mul %185, %233 : i64
    %235 = llvm.extractvalue %84[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %236 = llvm.mul %209, %235 : i64
    %237 = llvm.add %230, %232 : i64
    %238 = llvm.add %237, %234 : i64
    %239 = llvm.add %238, %236 : i64
    %240 = llvm.getelementptr %228[%239] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %227, %240 : f32, !llvm.ptr
    %241 = llvm.fadd %210, %227 : f32
    %242 = llvm.mlir.constant(1 : i64) : i64
    %243 = llvm.add %209, %242 : i64
    llvm.br ^bb28(%243, %241 : i64, f32)
  ^bb30:  // pred: ^bb28
    llvm.br ^bb31(%7 : i64)
  ^bb31(%244: i64):  // 2 preds: ^bb30, ^bb32
    %245 = llvm.icmp "slt" %244, %arg1 : i64
    llvm.cond_br %245, ^bb32, ^bb33
  ^bb32:  // pred: ^bb31
    %246 = llvm.extractvalue %84[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %247 = llvm.extractvalue %84[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %248 = llvm.mul %179, %247 : i64
    %249 = llvm.extractvalue %84[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %250 = llvm.mul %181, %249 : i64
    %251 = llvm.extractvalue %84[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %252 = llvm.mul %185, %251 : i64
    %253 = llvm.extractvalue %84[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %254 = llvm.mul %244, %253 : i64
    %255 = llvm.add %248, %250 : i64
    %256 = llvm.add %255, %252 : i64
    %257 = llvm.add %256, %254 : i64
    %258 = llvm.getelementptr %246[%257] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %259 = llvm.load %258 : !llvm.ptr -> f32
    %260 = llvm.fdiv %259, %210 : f32
    %261 = llvm.extractvalue %84[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %262 = llvm.extractvalue %84[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %263 = llvm.mul %179, %262 : i64
    %264 = llvm.extractvalue %84[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %265 = llvm.mul %181, %264 : i64
    %266 = llvm.extractvalue %84[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %267 = llvm.mul %185, %266 : i64
    %268 = llvm.extractvalue %84[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %269 = llvm.mul %244, %268 : i64
    %270 = llvm.add %263, %265 : i64
    %271 = llvm.add %270, %267 : i64
    %272 = llvm.add %271, %269 : i64
    %273 = llvm.getelementptr %261[%272] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %260, %273 : f32, !llvm.ptr
    %274 = llvm.mlir.constant(1 : i64) : i64
    %275 = llvm.add %244, %274 : i64
    llvm.br ^bb31(%275 : i64)
  ^bb33:  // pred: ^bb31
    %276 = llvm.mlir.constant(1 : i64) : i64
    %277 = llvm.add %185, %276 : i64
    llvm.br ^bb22(%277 : i64)
  ^bb34(%278: i64):  // 2 preds: ^bb18, ^bb39
    %279 = llvm.icmp "slt" %278, %arg0 : i64
    llvm.cond_br %279, ^bb35, ^bb36
  ^bb35:  // pred: ^bb34
    llvm.br ^bb37(%7 : i64)
  ^bb36:  // pred: ^bb34
    llvm.return
  ^bb37(%280: i64):  // 2 preds: ^bb35, ^bb51
    %281 = llvm.icmp "slt" %280, %arg1 : i64
    llvm.cond_br %281, ^bb38, ^bb39
  ^bb38:  // pred: ^bb37
    llvm.br ^bb40(%7 : i64)
  ^bb39:  // pred: ^bb37
    %282 = llvm.mlir.constant(1 : i64) : i64
    %283 = llvm.add %278, %282 : i64
    llvm.br ^bb34(%283 : i64)
  ^bb40(%284: i64):  // 2 preds: ^bb38, ^bb45
    %285 = llvm.icmp "slt" %284, %arg2 : i64
    llvm.cond_br %285, ^bb41, ^bb42
  ^bb41:  // pred: ^bb40
    llvm.br ^bb43(%7 : i64)
  ^bb42:  // pred: ^bb40
    llvm.br ^bb49(%7 : i64)
  ^bb43(%286: i64):  // 2 preds: ^bb41, ^bb48
    %287 = llvm.icmp "slt" %286, %arg3 : i64
    llvm.cond_br %287, ^bb44, ^bb45
  ^bb44:  // pred: ^bb43
    llvm.br ^bb46(%7, %9 : i64, f32)
  ^bb45:  // pred: ^bb43
    %288 = llvm.mlir.constant(1 : i64) : i64
    %289 = llvm.add %284, %288 : i64
    llvm.br ^bb40(%289 : i64)
  ^bb46(%290: i64, %291: f32):  // 2 preds: ^bb44, ^bb47
    %292 = llvm.icmp "slt" %290, %arg1 : i64
    llvm.cond_br %292, ^bb47, ^bb48
  ^bb47:  // pred: ^bb46
    %293 = llvm.extractvalue %84[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %294 = llvm.extractvalue %84[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %295 = llvm.mul %278, %294 : i64
    %296 = llvm.extractvalue %84[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %297 = llvm.mul %284, %296 : i64
    %298 = llvm.extractvalue %84[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %299 = llvm.mul %280, %298 : i64
    %300 = llvm.extractvalue %84[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %301 = llvm.mul %290, %300 : i64
    %302 = llvm.add %295, %297 : i64
    %303 = llvm.add %302, %299 : i64
    %304 = llvm.add %303, %301 : i64
    %305 = llvm.getelementptr %293[%304] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %306 = llvm.load %305 : !llvm.ptr -> f32
    %307 = llvm.extractvalue %56[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %308 = llvm.extractvalue %56[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %309 = llvm.mul %278, %308 : i64
    %310 = llvm.extractvalue %56[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %311 = llvm.mul %290, %310 : i64
    %312 = llvm.extractvalue %56[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %313 = llvm.mul %284, %312 : i64
    %314 = llvm.extractvalue %56[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %315 = llvm.mul %286, %314 : i64
    %316 = llvm.add %309, %311 : i64
    %317 = llvm.add %316, %313 : i64
    %318 = llvm.add %317, %315 : i64
    %319 = llvm.getelementptr %307[%318] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %320 = llvm.load %319 : !llvm.ptr -> f32
    %321 = llvm.fmul %306, %320 : f32
    %322 = llvm.fadd %291, %321 : f32
    %323 = llvm.mlir.constant(1 : i64) : i64
    %324 = llvm.add %290, %323 : i64
    llvm.br ^bb46(%324, %322 : i64, f32)
  ^bb48:  // pred: ^bb46
    %325 = llvm.extractvalue %98[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %326 = llvm.extractvalue %98[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %327 = llvm.mul %278, %326 : i64
    %328 = llvm.extractvalue %98[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %329 = llvm.mul %280, %328 : i64
    %330 = llvm.extractvalue %98[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %331 = llvm.mul %284, %330 : i64
    %332 = llvm.extractvalue %98[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %333 = llvm.mul %286, %332 : i64
    %334 = llvm.add %327, %329 : i64
    %335 = llvm.add %334, %331 : i64
    %336 = llvm.add %335, %333 : i64
    %337 = llvm.getelementptr %325[%336] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %291, %337 : f32, !llvm.ptr
    %338 = llvm.mlir.constant(1 : i64) : i64
    %339 = llvm.add %286, %338 : i64
    llvm.br ^bb43(%339 : i64)
  ^bb49(%340: i64):  // 2 preds: ^bb42, ^bb54
    %341 = llvm.icmp "slt" %340, %arg2 : i64
    llvm.cond_br %341, ^bb50, ^bb51
  ^bb50:  // pred: ^bb49
    llvm.br ^bb52(%7 : i64)
  ^bb51:  // pred: ^bb49
    %342 = llvm.mlir.constant(1 : i64) : i64
    %343 = llvm.add %280, %342 : i64
    llvm.br ^bb37(%343 : i64)
  ^bb52(%344: i64):  // 2 preds: ^bb50, ^bb53
    %345 = llvm.icmp "slt" %344, %arg3 : i64
    llvm.cond_br %345, ^bb53, ^bb54
  ^bb53:  // pred: ^bb52
    %346 = llvm.extractvalue %98[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %347 = llvm.extractvalue %98[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %348 = llvm.mul %278, %347 : i64
    %349 = llvm.extractvalue %98[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %350 = llvm.mul %280, %349 : i64
    %351 = llvm.extractvalue %98[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %352 = llvm.mul %340, %351 : i64
    %353 = llvm.extractvalue %98[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %354 = llvm.mul %344, %353 : i64
    %355 = llvm.add %348, %350 : i64
    %356 = llvm.add %355, %352 : i64
    %357 = llvm.add %356, %354 : i64
    %358 = llvm.getelementptr %346[%357] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %359 = llvm.load %358 : !llvm.ptr -> f32
    %360 = llvm.extractvalue %112[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %361 = llvm.extractvalue %112[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %362 = llvm.mul %278, %361 : i64
    %363 = llvm.extractvalue %112[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %364 = llvm.mul %280, %363 : i64
    %365 = llvm.extractvalue %112[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %366 = llvm.mul %340, %365 : i64
    %367 = llvm.extractvalue %112[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %368 = llvm.mul %344, %367 : i64
    %369 = llvm.add %362, %364 : i64
    %370 = llvm.add %369, %366 : i64
    %371 = llvm.add %370, %368 : i64
    %372 = llvm.getelementptr %360[%371] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %359, %372 : f32, !llvm.ptr
    %373 = llvm.mlir.constant(1 : i64) : i64
    %374 = llvm.add %344, %373 : i64
    llvm.br ^bb52(%374 : i64)
  ^bb54:  // pred: ^bb52
    %375 = llvm.mlir.constant(1 : i64) : i64
    %376 = llvm.add %340, %375 : i64
    llvm.br ^bb49(%376 : i64)
  }
}


module {
  llvm.func @bench_expf(f32) -> f32 attributes {sym_visibility = "private"}
  llvm.func @bench_inv_sqrt_index(i64) -> f32 attributes {sym_visibility = "private"}
  llvm.func @attention_mha(%arg0: i64, %arg1: i64, %arg2: i64, %arg3: i64, %arg4: !llvm.ptr, %arg5: !llvm.ptr, %arg6: i64, %arg7: i64, %arg8: i64, %arg9: !llvm.ptr, %arg10: !llvm.ptr, %arg11: i64, %arg12: i64, %arg13: i64, %arg14: !llvm.ptr, %arg15: !llvm.ptr, %arg16: i64, %arg17: i64, %arg18: i64, %arg19: !llvm.ptr, %arg20: !llvm.ptr, %arg21: i64, %arg22: i64, %arg23: i64, %arg24: !llvm.ptr, %arg25: !llvm.ptr, %arg26: i64, %arg27: i64, %arg28: i64, %arg29: !llvm.ptr, %arg30: !llvm.ptr, %arg31: i64, %arg32: i64, %arg33: i64, %arg34: !llvm.ptr, %arg35: !llvm.ptr, %arg36: i64, %arg37: i64, %arg38: i64) attributes {llvm.emit_c_interface} {
    %0 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %1 = llvm.insertvalue %arg34, %0[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %2 = llvm.insertvalue %arg35, %1[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %3 = llvm.insertvalue %arg36, %2[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %4 = llvm.insertvalue %arg37, %3[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %5 = llvm.insertvalue %arg38, %4[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %6 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %7 = llvm.insertvalue %arg29, %6[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %8 = llvm.insertvalue %arg30, %7[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %9 = llvm.insertvalue %arg31, %8[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %10 = llvm.insertvalue %arg32, %9[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %11 = llvm.insertvalue %arg33, %10[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %12 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %13 = llvm.insertvalue %arg24, %12[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %14 = llvm.insertvalue %arg25, %13[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %15 = llvm.insertvalue %arg26, %14[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %16 = llvm.insertvalue %arg27, %15[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %17 = llvm.insertvalue %arg28, %16[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %18 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %19 = llvm.insertvalue %arg19, %18[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %20 = llvm.insertvalue %arg20, %19[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %21 = llvm.insertvalue %arg21, %20[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %22 = llvm.insertvalue %arg22, %21[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %23 = llvm.insertvalue %arg23, %22[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %24 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %25 = llvm.insertvalue %arg14, %24[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %26 = llvm.insertvalue %arg15, %25[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %27 = llvm.insertvalue %arg16, %26[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %28 = llvm.insertvalue %arg17, %27[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %29 = llvm.insertvalue %arg18, %28[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %30 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %31 = llvm.insertvalue %arg9, %30[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %32 = llvm.insertvalue %arg10, %31[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %33 = llvm.insertvalue %arg11, %32[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %34 = llvm.insertvalue %arg12, %33[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %35 = llvm.insertvalue %arg13, %34[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %36 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %37 = llvm.insertvalue %arg4, %36[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %38 = llvm.insertvalue %arg5, %37[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %39 = llvm.insertvalue %arg6, %38[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %40 = llvm.insertvalue %arg7, %39[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %41 = llvm.insertvalue %arg8, %40[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %42 = llvm.mlir.constant(32 : index) : i64
    %43 = llvm.mlir.constant(0 : index) : i64
    %44 = llvm.mlir.constant(1 : index) : i64
    %45 = llvm.mlir.constant(0.000000e+00 : f32) : f32
    %46 = llvm.mlir.constant(-3.40282347E+38 : f32) : f32
    %47 = llvm.mul %arg2, %arg3 : i64
    %48 = llvm.mul %arg1, %47 : i64
    %49 = llvm.mul %arg1, %arg1 : i64
    %50 = llvm.mul %arg2, %49 : i64
    %51 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)>
    %52 = llvm.extractvalue %41[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %53 = llvm.extractvalue %41[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %54 = llvm.insertvalue %52, %51[0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %55 = llvm.insertvalue %53, %54[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %56 = llvm.insertvalue %43, %55[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %57 = llvm.insertvalue %arg0, %56[3, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %58 = llvm.insertvalue %48, %57[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %59 = llvm.insertvalue %arg1, %58[3, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %60 = llvm.insertvalue %47, %59[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %61 = llvm.insertvalue %47, %60[3, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %62 = llvm.insertvalue %44, %61[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %63 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)>
    %64 = llvm.extractvalue %35[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %65 = llvm.extractvalue %35[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %66 = llvm.insertvalue %64, %63[0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %67 = llvm.insertvalue %65, %66[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %68 = llvm.insertvalue %43, %67[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %69 = llvm.insertvalue %arg0, %68[3, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %70 = llvm.insertvalue %48, %69[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %71 = llvm.insertvalue %arg1, %70[3, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %72 = llvm.insertvalue %47, %71[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %73 = llvm.insertvalue %47, %72[3, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %74 = llvm.insertvalue %44, %73[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %75 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)>
    %76 = llvm.extractvalue %29[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %77 = llvm.extractvalue %29[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %78 = llvm.insertvalue %76, %75[0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %79 = llvm.insertvalue %77, %78[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %80 = llvm.insertvalue %43, %79[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %81 = llvm.insertvalue %arg0, %80[3, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %82 = llvm.insertvalue %48, %81[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %83 = llvm.insertvalue %arg1, %82[3, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %84 = llvm.insertvalue %47, %83[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %85 = llvm.insertvalue %47, %84[3, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %86 = llvm.insertvalue %44, %85[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %87 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %88 = llvm.extractvalue %23[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %89 = llvm.extractvalue %23[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %90 = llvm.insertvalue %88, %87[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %91 = llvm.insertvalue %89, %90[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %92 = llvm.insertvalue %43, %91[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %93 = llvm.insertvalue %arg0, %92[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %94 = llvm.insertvalue %50, %93[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %95 = llvm.insertvalue %arg2, %94[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %96 = llvm.insertvalue %49, %95[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %97 = llvm.insertvalue %arg1, %96[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %98 = llvm.insertvalue %arg1, %97[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %99 = llvm.insertvalue %arg1, %98[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %100 = llvm.insertvalue %44, %99[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %101 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %102 = llvm.extractvalue %17[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %103 = llvm.extractvalue %17[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %104 = llvm.insertvalue %102, %101[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %105 = llvm.insertvalue %103, %104[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %106 = llvm.insertvalue %43, %105[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %107 = llvm.insertvalue %arg0, %106[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %108 = llvm.insertvalue %50, %107[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %109 = llvm.insertvalue %arg2, %108[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %110 = llvm.insertvalue %49, %109[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %111 = llvm.insertvalue %arg1, %110[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %112 = llvm.insertvalue %arg1, %111[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %113 = llvm.insertvalue %arg1, %112[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %114 = llvm.insertvalue %44, %113[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %115 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)>
    %116 = llvm.extractvalue %5[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %117 = llvm.extractvalue %5[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %118 = llvm.insertvalue %116, %115[0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %119 = llvm.insertvalue %117, %118[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %120 = llvm.insertvalue %43, %119[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %121 = llvm.insertvalue %arg0, %120[3, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %122 = llvm.insertvalue %48, %121[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %123 = llvm.insertvalue %arg1, %122[3, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %124 = llvm.insertvalue %47, %123[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %125 = llvm.insertvalue %47, %124[3, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %126 = llvm.insertvalue %44, %125[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %127 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)>
    %128 = llvm.extractvalue %11[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %129 = llvm.extractvalue %11[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %130 = llvm.insertvalue %128, %127[0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %131 = llvm.insertvalue %129, %130[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %132 = llvm.insertvalue %43, %131[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %133 = llvm.insertvalue %arg0, %132[3, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %134 = llvm.insertvalue %48, %133[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %135 = llvm.insertvalue %arg1, %134[3, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %136 = llvm.insertvalue %47, %135[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %137 = llvm.insertvalue %47, %136[3, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %138 = llvm.insertvalue %44, %137[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %139 = llvm.call @bench_inv_sqrt_index(%arg3) : (i64) -> f32
    llvm.br ^bb1(%43 : i64)
  ^bb1(%140: i64):  // 2 preds: ^bb0, ^bb20
    %141 = llvm.icmp "slt" %140, %arg0 : i64
    llvm.cond_br %141, ^bb2, ^bb21
  ^bb2:  // pred: ^bb1
    llvm.br ^bb3(%43 : i64)
  ^bb3(%142: i64):  // 2 preds: ^bb2, ^bb19
    %143 = llvm.icmp "slt" %142, %arg2 : i64
    llvm.cond_br %143, ^bb4, ^bb20
  ^bb4:  // pred: ^bb3
    %144 = llvm.add %140, %42 : i64
    %145 = llvm.intr.smin(%144, %arg0) : (i64, i64) -> i64
    llvm.br ^bb5(%140 : i64)
  ^bb5(%146: i64):  // 2 preds: ^bb4, ^bb18
    %147 = llvm.icmp "slt" %146, %145 : i64
    llvm.cond_br %147, ^bb6, ^bb19
  ^bb6:  // pred: ^bb5
    %148 = llvm.add %142, %42 : i64
    %149 = llvm.intr.smin(%148, %arg2) : (i64, i64) -> i64
    llvm.br ^bb7(%142 : i64)
  ^bb7(%150: i64):  // 2 preds: ^bb6, ^bb17
    %151 = llvm.icmp "slt" %150, %149 : i64
    llvm.cond_br %151, ^bb8, ^bb18
  ^bb8:  // pred: ^bb7
    %152 = llvm.mul %150, %arg3 : i64
    llvm.br ^bb9(%43 : i64)
  ^bb9(%153: i64):  // 2 preds: ^bb8, ^bb16
    %154 = llvm.icmp "slt" %153, %arg1 : i64
    llvm.cond_br %154, ^bb10, ^bb17
  ^bb10:  // pred: ^bb9
    llvm.br ^bb11(%43 : i64)
  ^bb11(%155: i64):  // 2 preds: ^bb10, ^bb15
    %156 = llvm.icmp "slt" %155, %arg1 : i64
    llvm.cond_br %156, ^bb12, ^bb16
  ^bb12:  // pred: ^bb11
    llvm.br ^bb13(%43, %45 : i64, f32)
  ^bb13(%157: i64, %158: f32):  // 2 preds: ^bb12, ^bb14
    %159 = llvm.icmp "slt" %157, %arg3 : i64
    llvm.cond_br %159, ^bb14, ^bb15
  ^bb14:  // pred: ^bb13
    %160 = llvm.add %152, %157 : i64
    %161 = llvm.extractvalue %62[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %162 = llvm.extractvalue %62[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %163 = llvm.getelementptr %161[%162] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %164 = llvm.extractvalue %62[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %165 = llvm.mul %146, %164 overflow<nsw, nuw> : i64
    %166 = llvm.extractvalue %62[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %167 = llvm.mul %153, %166 overflow<nsw, nuw> : i64
    %168 = llvm.add %165, %167 overflow<nsw, nuw> : i64
    %169 = llvm.extractvalue %62[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %170 = llvm.mul %160, %169 overflow<nsw, nuw> : i64
    %171 = llvm.add %168, %170 overflow<nsw, nuw> : i64
    %172 = llvm.getelementptr inbounds|nuw %163[%171] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %173 = llvm.load %172 : !llvm.ptr -> f32
    %174 = llvm.extractvalue %74[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %175 = llvm.extractvalue %74[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %176 = llvm.getelementptr %174[%175] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %177 = llvm.extractvalue %74[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %178 = llvm.mul %146, %177 overflow<nsw, nuw> : i64
    %179 = llvm.extractvalue %74[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %180 = llvm.mul %155, %179 overflow<nsw, nuw> : i64
    %181 = llvm.add %178, %180 overflow<nsw, nuw> : i64
    %182 = llvm.extractvalue %74[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %183 = llvm.mul %160, %182 overflow<nsw, nuw> : i64
    %184 = llvm.add %181, %183 overflow<nsw, nuw> : i64
    %185 = llvm.getelementptr inbounds|nuw %176[%184] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %186 = llvm.load %185 : !llvm.ptr -> f32
    %187 = llvm.fmul %173, %186 : f32
    %188 = llvm.fadd %158, %187 : f32
    %189 = llvm.add %157, %44 : i64
    llvm.br ^bb13(%189, %188 : i64, f32)
  ^bb15:  // pred: ^bb13
    %190 = llvm.fmul %158, %139 : f32
    %191 = llvm.extractvalue %100[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %192 = llvm.extractvalue %100[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %193 = llvm.getelementptr %191[%192] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %194 = llvm.extractvalue %100[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %195 = llvm.mul %146, %194 overflow<nsw, nuw> : i64
    %196 = llvm.extractvalue %100[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %197 = llvm.mul %150, %196 overflow<nsw, nuw> : i64
    %198 = llvm.add %195, %197 overflow<nsw, nuw> : i64
    %199 = llvm.extractvalue %100[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %200 = llvm.mul %153, %199 overflow<nsw, nuw> : i64
    %201 = llvm.add %198, %200 overflow<nsw, nuw> : i64
    %202 = llvm.extractvalue %100[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %203 = llvm.mul %155, %202 overflow<nsw, nuw> : i64
    %204 = llvm.add %201, %203 overflow<nsw, nuw> : i64
    %205 = llvm.getelementptr inbounds|nuw %193[%204] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %190, %205 : f32, !llvm.ptr
    %206 = llvm.add %155, %44 : i64
    llvm.br ^bb11(%206 : i64)
  ^bb16:  // pred: ^bb11
    %207 = llvm.add %153, %44 : i64
    llvm.br ^bb9(%207 : i64)
  ^bb17:  // pred: ^bb9
    %208 = llvm.add %150, %44 : i64
    llvm.br ^bb7(%208 : i64)
  ^bb18:  // pred: ^bb7
    %209 = llvm.add %146, %44 : i64
    llvm.br ^bb5(%209 : i64)
  ^bb19:  // pred: ^bb5
    %210 = llvm.add %142, %42 : i64
    llvm.br ^bb3(%210 : i64)
  ^bb20:  // pred: ^bb3
    %211 = llvm.add %140, %42 : i64
    llvm.br ^bb1(%211 : i64)
  ^bb21:  // pred: ^bb1
    llvm.br ^bb22(%43 : i64)
  ^bb22(%212: i64):  // 2 preds: ^bb21, ^bb47
    %213 = llvm.icmp "slt" %212, %arg0 : i64
    llvm.cond_br %213, ^bb23, ^bb48
  ^bb23:  // pred: ^bb22
    llvm.br ^bb24(%43 : i64)
  ^bb24(%214: i64):  // 2 preds: ^bb23, ^bb46
    %215 = llvm.icmp "slt" %214, %arg2 : i64
    llvm.cond_br %215, ^bb25, ^bb47
  ^bb25:  // pred: ^bb24
    llvm.br ^bb26(%43 : i64)
  ^bb26(%216: i64):  // 2 preds: ^bb25, ^bb45
    %217 = llvm.icmp "slt" %216, %arg1 : i64
    llvm.cond_br %217, ^bb27, ^bb46
  ^bb27:  // pred: ^bb26
    %218 = llvm.add %212, %42 : i64
    %219 = llvm.intr.smin(%218, %arg0) : (i64, i64) -> i64
    llvm.br ^bb28(%212 : i64)
  ^bb28(%220: i64):  // 2 preds: ^bb27, ^bb44
    %221 = llvm.icmp "slt" %220, %219 : i64
    llvm.cond_br %221, ^bb29, ^bb45
  ^bb29:  // pred: ^bb28
    %222 = llvm.add %214, %42 : i64
    %223 = llvm.intr.smin(%222, %arg2) : (i64, i64) -> i64
    llvm.br ^bb30(%214 : i64)
  ^bb30(%224: i64):  // 2 preds: ^bb29, ^bb43
    %225 = llvm.icmp "slt" %224, %223 : i64
    llvm.cond_br %225, ^bb31, ^bb44
  ^bb31:  // pred: ^bb30
    %226 = llvm.add %216, %42 : i64
    %227 = llvm.intr.smin(%226, %arg1) : (i64, i64) -> i64
    llvm.br ^bb32(%216 : i64)
  ^bb32(%228: i64):  // 2 preds: ^bb31, ^bb42
    %229 = llvm.icmp "slt" %228, %227 : i64
    llvm.cond_br %229, ^bb33, ^bb43
  ^bb33:  // pred: ^bb32
    llvm.br ^bb34(%43, %46 : i64, f32)
  ^bb34(%230: i64, %231: f32):  // 2 preds: ^bb33, ^bb35
    %232 = llvm.icmp "slt" %230, %arg1 : i64
    llvm.cond_br %232, ^bb35, ^bb36
  ^bb35:  // pred: ^bb34
    %233 = llvm.extractvalue %100[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %234 = llvm.extractvalue %100[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %235 = llvm.getelementptr %233[%234] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %236 = llvm.extractvalue %100[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %237 = llvm.mul %220, %236 overflow<nsw, nuw> : i64
    %238 = llvm.extractvalue %100[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %239 = llvm.mul %224, %238 overflow<nsw, nuw> : i64
    %240 = llvm.add %237, %239 overflow<nsw, nuw> : i64
    %241 = llvm.extractvalue %100[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %242 = llvm.mul %228, %241 overflow<nsw, nuw> : i64
    %243 = llvm.add %240, %242 overflow<nsw, nuw> : i64
    %244 = llvm.extractvalue %100[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %245 = llvm.mul %230, %244 overflow<nsw, nuw> : i64
    %246 = llvm.add %243, %245 overflow<nsw, nuw> : i64
    %247 = llvm.getelementptr inbounds|nuw %235[%246] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %248 = llvm.load %247 : !llvm.ptr -> f32
    %249 = llvm.intr.maximum(%231, %248) : (f32, f32) -> f32
    %250 = llvm.add %230, %44 : i64
    llvm.br ^bb34(%250, %249 : i64, f32)
  ^bb36:  // pred: ^bb34
    llvm.br ^bb37(%43, %45 : i64, f32)
  ^bb37(%251: i64, %252: f32):  // 2 preds: ^bb36, ^bb38
    %253 = llvm.icmp "slt" %251, %arg1 : i64
    llvm.cond_br %253, ^bb38, ^bb39
  ^bb38:  // pred: ^bb37
    %254 = llvm.extractvalue %100[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %255 = llvm.extractvalue %100[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %256 = llvm.getelementptr %254[%255] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %257 = llvm.extractvalue %100[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %258 = llvm.mul %220, %257 overflow<nsw, nuw> : i64
    %259 = llvm.extractvalue %100[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %260 = llvm.mul %224, %259 overflow<nsw, nuw> : i64
    %261 = llvm.add %258, %260 overflow<nsw, nuw> : i64
    %262 = llvm.extractvalue %100[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %263 = llvm.mul %228, %262 overflow<nsw, nuw> : i64
    %264 = llvm.add %261, %263 overflow<nsw, nuw> : i64
    %265 = llvm.extractvalue %100[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %266 = llvm.mul %251, %265 overflow<nsw, nuw> : i64
    %267 = llvm.add %264, %266 overflow<nsw, nuw> : i64
    %268 = llvm.getelementptr inbounds|nuw %256[%267] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %269 = llvm.load %268 : !llvm.ptr -> f32
    %270 = llvm.fsub %269, %231 : f32
    %271 = llvm.call @bench_expf(%270) : (f32) -> f32
    %272 = llvm.extractvalue %114[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %273 = llvm.extractvalue %114[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %274 = llvm.getelementptr %272[%273] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %275 = llvm.extractvalue %114[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %276 = llvm.mul %220, %275 overflow<nsw, nuw> : i64
    %277 = llvm.extractvalue %114[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %278 = llvm.mul %224, %277 overflow<nsw, nuw> : i64
    %279 = llvm.add %276, %278 overflow<nsw, nuw> : i64
    %280 = llvm.extractvalue %114[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %281 = llvm.mul %228, %280 overflow<nsw, nuw> : i64
    %282 = llvm.add %279, %281 overflow<nsw, nuw> : i64
    %283 = llvm.extractvalue %114[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %284 = llvm.mul %251, %283 overflow<nsw, nuw> : i64
    %285 = llvm.add %282, %284 overflow<nsw, nuw> : i64
    %286 = llvm.getelementptr inbounds|nuw %274[%285] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %271, %286 : f32, !llvm.ptr
    %287 = llvm.fadd %252, %271 : f32
    %288 = llvm.add %251, %44 : i64
    llvm.br ^bb37(%288, %287 : i64, f32)
  ^bb39:  // pred: ^bb37
    llvm.br ^bb40(%43 : i64)
  ^bb40(%289: i64):  // 2 preds: ^bb39, ^bb41
    %290 = llvm.icmp "slt" %289, %arg1 : i64
    llvm.cond_br %290, ^bb41, ^bb42
  ^bb41:  // pred: ^bb40
    %291 = llvm.extractvalue %114[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %292 = llvm.extractvalue %114[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %293 = llvm.getelementptr %291[%292] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %294 = llvm.extractvalue %114[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %295 = llvm.mul %220, %294 overflow<nsw, nuw> : i64
    %296 = llvm.extractvalue %114[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %297 = llvm.mul %224, %296 overflow<nsw, nuw> : i64
    %298 = llvm.add %295, %297 overflow<nsw, nuw> : i64
    %299 = llvm.extractvalue %114[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %300 = llvm.mul %228, %299 overflow<nsw, nuw> : i64
    %301 = llvm.add %298, %300 overflow<nsw, nuw> : i64
    %302 = llvm.extractvalue %114[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %303 = llvm.mul %289, %302 overflow<nsw, nuw> : i64
    %304 = llvm.add %301, %303 overflow<nsw, nuw> : i64
    %305 = llvm.getelementptr inbounds|nuw %293[%304] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %306 = llvm.load %305 : !llvm.ptr -> f32
    %307 = llvm.fdiv %306, %252 : f32
    %308 = llvm.extractvalue %114[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %309 = llvm.extractvalue %114[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %310 = llvm.getelementptr %308[%309] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %311 = llvm.extractvalue %114[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %312 = llvm.mul %220, %311 overflow<nsw, nuw> : i64
    %313 = llvm.extractvalue %114[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %314 = llvm.mul %224, %313 overflow<nsw, nuw> : i64
    %315 = llvm.add %312, %314 overflow<nsw, nuw> : i64
    %316 = llvm.extractvalue %114[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %317 = llvm.mul %228, %316 overflow<nsw, nuw> : i64
    %318 = llvm.add %315, %317 overflow<nsw, nuw> : i64
    %319 = llvm.extractvalue %114[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %320 = llvm.mul %289, %319 overflow<nsw, nuw> : i64
    %321 = llvm.add %318, %320 overflow<nsw, nuw> : i64
    %322 = llvm.getelementptr inbounds|nuw %310[%321] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %307, %322 : f32, !llvm.ptr
    %323 = llvm.add %289, %44 : i64
    llvm.br ^bb40(%323 : i64)
  ^bb42:  // pred: ^bb40
    %324 = llvm.add %228, %44 : i64
    llvm.br ^bb32(%324 : i64)
  ^bb43:  // pred: ^bb32
    %325 = llvm.add %224, %44 : i64
    llvm.br ^bb30(%325 : i64)
  ^bb44:  // pred: ^bb30
    %326 = llvm.add %220, %44 : i64
    llvm.br ^bb28(%326 : i64)
  ^bb45:  // pred: ^bb28
    %327 = llvm.add %216, %42 : i64
    llvm.br ^bb26(%327 : i64)
  ^bb46:  // pred: ^bb26
    %328 = llvm.add %214, %42 : i64
    llvm.br ^bb24(%328 : i64)
  ^bb47:  // pred: ^bb24
    %329 = llvm.add %212, %42 : i64
    llvm.br ^bb22(%329 : i64)
  ^bb48:  // pred: ^bb22
    llvm.br ^bb49(%43 : i64)
  ^bb49(%330: i64):  // 2 preds: ^bb48, ^bb71
    %331 = llvm.icmp "slt" %330, %arg0 : i64
    llvm.cond_br %331, ^bb50, ^bb72
  ^bb50:  // pred: ^bb49
    llvm.br ^bb51(%43 : i64)
  ^bb51(%332: i64):  // 2 preds: ^bb50, ^bb70
    %333 = llvm.icmp "slt" %332, %arg1 : i64
    llvm.cond_br %333, ^bb52, ^bb71
  ^bb52:  // pred: ^bb51
    llvm.br ^bb53(%43 : i64)
  ^bb53(%334: i64):  // 2 preds: ^bb52, ^bb69
    %335 = llvm.icmp "slt" %334, %arg2 : i64
    llvm.cond_br %335, ^bb54, ^bb70
  ^bb54:  // pred: ^bb53
    %336 = llvm.add %330, %42 : i64
    %337 = llvm.intr.smin(%336, %arg0) : (i64, i64) -> i64
    llvm.br ^bb55(%330 : i64)
  ^bb55(%338: i64):  // 2 preds: ^bb54, ^bb68
    %339 = llvm.icmp "slt" %338, %337 : i64
    llvm.cond_br %339, ^bb56, ^bb69
  ^bb56:  // pred: ^bb55
    %340 = llvm.add %332, %42 : i64
    %341 = llvm.intr.smin(%340, %arg1) : (i64, i64) -> i64
    llvm.br ^bb57(%332 : i64)
  ^bb57(%342: i64):  // 2 preds: ^bb56, ^bb67
    %343 = llvm.icmp "slt" %342, %341 : i64
    llvm.cond_br %343, ^bb58, ^bb68
  ^bb58:  // pred: ^bb57
    %344 = llvm.add %334, %42 : i64
    %345 = llvm.intr.smin(%344, %arg2) : (i64, i64) -> i64
    llvm.br ^bb59(%334 : i64)
  ^bb59(%346: i64):  // 2 preds: ^bb58, ^bb66
    %347 = llvm.icmp "slt" %346, %345 : i64
    llvm.cond_br %347, ^bb60, ^bb67
  ^bb60:  // pred: ^bb59
    %348 = llvm.mul %346, %arg3 : i64
    llvm.br ^bb61(%43 : i64)
  ^bb61(%349: i64):  // 2 preds: ^bb60, ^bb65
    %350 = llvm.icmp "slt" %349, %arg3 : i64
    llvm.cond_br %350, ^bb62, ^bb66
  ^bb62:  // pred: ^bb61
    %351 = llvm.add %348, %349 : i64
    llvm.br ^bb63(%43, %45 : i64, f32)
  ^bb63(%352: i64, %353: f32):  // 2 preds: ^bb62, ^bb64
    %354 = llvm.icmp "slt" %352, %arg1 : i64
    llvm.cond_br %354, ^bb64, ^bb65
  ^bb64:  // pred: ^bb63
    %355 = llvm.extractvalue %114[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %356 = llvm.extractvalue %114[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %357 = llvm.getelementptr %355[%356] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %358 = llvm.extractvalue %114[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %359 = llvm.mul %338, %358 overflow<nsw, nuw> : i64
    %360 = llvm.extractvalue %114[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %361 = llvm.mul %346, %360 overflow<nsw, nuw> : i64
    %362 = llvm.add %359, %361 overflow<nsw, nuw> : i64
    %363 = llvm.extractvalue %114[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %364 = llvm.mul %342, %363 overflow<nsw, nuw> : i64
    %365 = llvm.add %362, %364 overflow<nsw, nuw> : i64
    %366 = llvm.extractvalue %114[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %367 = llvm.mul %352, %366 overflow<nsw, nuw> : i64
    %368 = llvm.add %365, %367 overflow<nsw, nuw> : i64
    %369 = llvm.getelementptr inbounds|nuw %357[%368] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %370 = llvm.load %369 : !llvm.ptr -> f32
    %371 = llvm.extractvalue %86[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %372 = llvm.extractvalue %86[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %373 = llvm.getelementptr %371[%372] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %374 = llvm.extractvalue %86[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %375 = llvm.mul %338, %374 overflow<nsw, nuw> : i64
    %376 = llvm.extractvalue %86[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %377 = llvm.mul %352, %376 overflow<nsw, nuw> : i64
    %378 = llvm.add %375, %377 overflow<nsw, nuw> : i64
    %379 = llvm.extractvalue %86[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %380 = llvm.mul %351, %379 overflow<nsw, nuw> : i64
    %381 = llvm.add %378, %380 overflow<nsw, nuw> : i64
    %382 = llvm.getelementptr inbounds|nuw %373[%381] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %383 = llvm.load %382 : !llvm.ptr -> f32
    %384 = llvm.fmul %370, %383 : f32
    %385 = llvm.fadd %353, %384 : f32
    %386 = llvm.add %352, %44 : i64
    llvm.br ^bb63(%386, %385 : i64, f32)
  ^bb65:  // pred: ^bb63
    %387 = llvm.extractvalue %138[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %388 = llvm.extractvalue %138[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %389 = llvm.getelementptr %387[%388] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %390 = llvm.extractvalue %138[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %391 = llvm.mul %338, %390 overflow<nsw, nuw> : i64
    %392 = llvm.extractvalue %138[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %393 = llvm.mul %342, %392 overflow<nsw, nuw> : i64
    %394 = llvm.add %391, %393 overflow<nsw, nuw> : i64
    %395 = llvm.extractvalue %138[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %396 = llvm.mul %351, %395 overflow<nsw, nuw> : i64
    %397 = llvm.add %394, %396 overflow<nsw, nuw> : i64
    %398 = llvm.getelementptr inbounds|nuw %389[%397] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %353, %398 : f32, !llvm.ptr
    %399 = llvm.add %349, %44 : i64
    llvm.br ^bb61(%399 : i64)
  ^bb66:  // pred: ^bb61
    %400 = llvm.add %346, %44 : i64
    llvm.br ^bb59(%400 : i64)
  ^bb67:  // pred: ^bb59
    %401 = llvm.add %342, %44 : i64
    llvm.br ^bb57(%401 : i64)
  ^bb68:  // pred: ^bb57
    %402 = llvm.add %338, %44 : i64
    llvm.br ^bb55(%402 : i64)
  ^bb69:  // pred: ^bb55
    %403 = llvm.add %334, %42 : i64
    llvm.br ^bb53(%403 : i64)
  ^bb70:  // pred: ^bb53
    %404 = llvm.add %332, %42 : i64
    llvm.br ^bb51(%404 : i64)
  ^bb71:  // pred: ^bb51
    %405 = llvm.add %330, %42 : i64
    llvm.br ^bb49(%405 : i64)
  ^bb72:  // pred: ^bb49
    llvm.br ^bb73(%43 : i64)
  ^bb73(%406: i64):  // 2 preds: ^bb72, ^bb80
    %407 = llvm.icmp "slt" %406, %arg0 : i64
    llvm.cond_br %407, ^bb74, ^bb81
  ^bb74:  // pred: ^bb73
    llvm.br ^bb75(%43 : i64)
  ^bb75(%408: i64):  // 2 preds: ^bb74, ^bb79
    %409 = llvm.icmp "slt" %408, %arg1 : i64
    llvm.cond_br %409, ^bb76, ^bb80
  ^bb76:  // pred: ^bb75
    llvm.br ^bb77(%43 : i64)
  ^bb77(%410: i64):  // 2 preds: ^bb76, ^bb78
    %411 = llvm.icmp "slt" %410, %47 : i64
    llvm.cond_br %411, ^bb78, ^bb79
  ^bb78:  // pred: ^bb77
    %412 = llvm.extractvalue %138[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %413 = llvm.extractvalue %138[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %414 = llvm.getelementptr %412[%413] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %415 = llvm.extractvalue %138[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %416 = llvm.mul %406, %415 overflow<nsw, nuw> : i64
    %417 = llvm.extractvalue %138[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %418 = llvm.mul %408, %417 overflow<nsw, nuw> : i64
    %419 = llvm.add %416, %418 overflow<nsw, nuw> : i64
    %420 = llvm.extractvalue %138[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %421 = llvm.mul %410, %420 overflow<nsw, nuw> : i64
    %422 = llvm.add %419, %421 overflow<nsw, nuw> : i64
    %423 = llvm.getelementptr inbounds|nuw %414[%422] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %424 = llvm.load %423 : !llvm.ptr -> f32
    %425 = llvm.extractvalue %126[1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %426 = llvm.extractvalue %126[2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %427 = llvm.getelementptr %425[%426] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %428 = llvm.extractvalue %126[4, 0] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %429 = llvm.mul %406, %428 overflow<nsw, nuw> : i64
    %430 = llvm.extractvalue %126[4, 1] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %431 = llvm.mul %408, %430 overflow<nsw, nuw> : i64
    %432 = llvm.add %429, %431 overflow<nsw, nuw> : i64
    %433 = llvm.extractvalue %126[4, 2] : !llvm.struct<(ptr, ptr, i64, array<3 x i64>, array<3 x i64>)> 
    %434 = llvm.mul %410, %433 overflow<nsw, nuw> : i64
    %435 = llvm.add %432, %434 overflow<nsw, nuw> : i64
    %436 = llvm.getelementptr inbounds|nuw %427[%435] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %424, %436 : f32, !llvm.ptr
    %437 = llvm.add %410, %44 : i64
    llvm.br ^bb77(%437 : i64)
  ^bb79:  // pred: ^bb77
    %438 = llvm.add %408, %44 : i64
    llvm.br ^bb75(%438 : i64)
  ^bb80:  // pred: ^bb75
    %439 = llvm.add %406, %44 : i64
    llvm.br ^bb73(%439 : i64)
  ^bb81:  // pred: ^bb73
    llvm.return
  }
  llvm.func @_mlir_ciface_attention_mha(%arg0: i64, %arg1: i64, %arg2: i64, %arg3: i64, %arg4: !llvm.ptr, %arg5: !llvm.ptr, %arg6: !llvm.ptr, %arg7: !llvm.ptr, %arg8: !llvm.ptr, %arg9: !llvm.ptr, %arg10: !llvm.ptr) attributes {llvm.emit_c_interface} {
    %0 = llvm.load %arg4 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %1 = llvm.extractvalue %0[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %2 = llvm.extractvalue %0[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %3 = llvm.extractvalue %0[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %4 = llvm.extractvalue %0[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %5 = llvm.extractvalue %0[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %6 = llvm.load %arg5 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %7 = llvm.extractvalue %6[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %8 = llvm.extractvalue %6[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %9 = llvm.extractvalue %6[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %10 = llvm.extractvalue %6[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %11 = llvm.extractvalue %6[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %12 = llvm.load %arg6 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %13 = llvm.extractvalue %12[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %14 = llvm.extractvalue %12[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %15 = llvm.extractvalue %12[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %16 = llvm.extractvalue %12[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %17 = llvm.extractvalue %12[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %18 = llvm.load %arg7 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %19 = llvm.extractvalue %18[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %20 = llvm.extractvalue %18[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %21 = llvm.extractvalue %18[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %22 = llvm.extractvalue %18[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %23 = llvm.extractvalue %18[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %24 = llvm.load %arg8 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %25 = llvm.extractvalue %24[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %26 = llvm.extractvalue %24[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %27 = llvm.extractvalue %24[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %28 = llvm.extractvalue %24[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %29 = llvm.extractvalue %24[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %30 = llvm.load %arg9 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %31 = llvm.extractvalue %30[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %32 = llvm.extractvalue %30[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %33 = llvm.extractvalue %30[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %34 = llvm.extractvalue %30[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %35 = llvm.extractvalue %30[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %36 = llvm.load %arg10 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %37 = llvm.extractvalue %36[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %38 = llvm.extractvalue %36[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %39 = llvm.extractvalue %36[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %40 = llvm.extractvalue %36[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %41 = llvm.extractvalue %36[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    llvm.call @attention_mha(%arg0, %arg1, %arg2, %arg3, %1, %2, %3, %4, %5, %7, %8, %9, %10, %11, %13, %14, %15, %16, %17, %19, %20, %21, %22, %23, %25, %26, %27, %28, %29, %31, %32, %33, %34, %35, %37, %38, %39, %40, %41) : (i64, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64) -> ()
    llvm.return
  }
}


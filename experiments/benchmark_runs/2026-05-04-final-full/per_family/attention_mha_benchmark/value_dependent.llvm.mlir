module {
  llvm.func @bench_expf(f32) -> f32
  llvm.func @bench_inv_sqrt_index(i64) -> f32
  llvm.func @attention_mha(%arg0: i64, %arg1: i64, %arg2: i64, %arg3: i64, %arg4: !llvm.ptr, %arg5: !llvm.ptr, %arg6: !llvm.ptr, %arg7: !llvm.ptr, %arg8: !llvm.ptr, %arg9: !llvm.ptr, %arg10: !llvm.ptr) {
    %0 = llvm.mlir.constant(1 : i64) : i64
    %1 = llvm.mlir.constant(0 : i64) : i64
    %2 = llvm.mlir.constant(0.000000e+00 : f32) : f32
    %3 = llvm.mlir.constant(-3.40282347E+38 : f32) : f32
    %4 = llvm.mul %arg2, %arg3 : i64
    %5 = llvm.mul %arg1, %4 : i64
    %6 = llvm.mul %arg1, %arg1 : i64
    %7 = llvm.mul %arg2, %6 : i64
    %8 = llvm.call @bench_inv_sqrt_index(%arg3) : (i64) -> f32
    llvm.br ^bb1(%1 : i64)
  ^bb1(%9: i64):  // 2 preds: ^bb0, ^bb6
    %10 = llvm.icmp "slt" %9, %arg0 : i64
    llvm.cond_br %10, ^bb2, ^bb3
  ^bb2:  // pred: ^bb1
    llvm.br ^bb4(%1 : i64)
  ^bb3:  // pred: ^bb1
    llvm.br ^bb16(%1 : i64)
  ^bb4(%11: i64):  // 2 preds: ^bb2, ^bb9
    %12 = llvm.icmp "slt" %11, %arg2 : i64
    llvm.cond_br %12, ^bb5, ^bb6
  ^bb5:  // pred: ^bb4
    %13 = llvm.mul %11, %arg3 : i64
    llvm.br ^bb7(%1 : i64)
  ^bb6:  // pred: ^bb4
    %14 = llvm.add %9, %0 : i64
    llvm.br ^bb1(%14 : i64)
  ^bb7(%15: i64):  // 2 preds: ^bb5, ^bb12
    %16 = llvm.icmp "slt" %15, %arg1 : i64
    llvm.cond_br %16, ^bb8, ^bb9
  ^bb8:  // pred: ^bb7
    llvm.br ^bb10(%1 : i64)
  ^bb9:  // pred: ^bb7
    %17 = llvm.add %11, %0 : i64
    llvm.br ^bb4(%17 : i64)
  ^bb10(%18: i64):  // 2 preds: ^bb8, ^bb15
    %19 = llvm.icmp "slt" %18, %arg1 : i64
    llvm.cond_br %19, ^bb11, ^bb12
  ^bb11:  // pred: ^bb10
    llvm.br ^bb13(%1, %2 : i64, f32)
  ^bb12:  // pred: ^bb10
    %20 = llvm.add %15, %0 : i64
    llvm.br ^bb7(%20 : i64)
  ^bb13(%21: i64, %22: f32):  // 2 preds: ^bb11, ^bb14
    %23 = llvm.icmp "slt" %21, %arg3 : i64
    llvm.cond_br %23, ^bb14, ^bb15
  ^bb14:  // pred: ^bb13
    %24 = llvm.add %13, %21 : i64
    %25 = llvm.mul %9, %5 : i64
    %26 = llvm.mul %15, %4 : i64
    %27 = llvm.add %25, %26 : i64
    %28 = llvm.add %27, %24 : i64
    %29 = llvm.getelementptr %arg4[%28] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %30 = llvm.load %29 : !llvm.ptr -> f32
    %31 = llvm.mul %9, %5 : i64
    %32 = llvm.mul %18, %4 : i64
    %33 = llvm.add %31, %32 : i64
    %34 = llvm.add %33, %24 : i64
    %35 = llvm.getelementptr %arg5[%34] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %36 = llvm.load %35 : !llvm.ptr -> f32
    %37 = llvm.fmul %30, %36 : f32
    %38 = llvm.fadd %22, %37 : f32
    %39 = llvm.add %21, %0 : i64
    llvm.br ^bb13(%39, %38 : i64, f32)
  ^bb15:  // pred: ^bb13
    %40 = llvm.fmul %22, %8 : f32
    %41 = llvm.mul %9, %7 : i64
    %42 = llvm.mul %11, %6 : i64
    %43 = llvm.mul %15, %arg1 : i64
    %44 = llvm.add %41, %42 : i64
    %45 = llvm.add %44, %43 : i64
    %46 = llvm.add %45, %18 : i64
    %47 = llvm.getelementptr %arg7[%46] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %40, %47 : f32, !llvm.ptr
    %48 = llvm.add %18, %0 : i64
    llvm.br ^bb10(%48 : i64)
  ^bb16(%49: i64):  // 2 preds: ^bb3, ^bb21
    %50 = llvm.icmp "slt" %49, %arg0 : i64
    llvm.cond_br %50, ^bb17, ^bb18
  ^bb17:  // pred: ^bb16
    llvm.br ^bb19(%1 : i64)
  ^bb18:  // pred: ^bb16
    llvm.br ^bb34(%1 : i64)
  ^bb19(%51: i64):  // 2 preds: ^bb17, ^bb24
    %52 = llvm.icmp "slt" %51, %arg2 : i64
    llvm.cond_br %52, ^bb20, ^bb21
  ^bb20:  // pred: ^bb19
    llvm.br ^bb22(%1 : i64)
  ^bb21:  // pred: ^bb19
    %53 = llvm.add %49, %0 : i64
    llvm.br ^bb16(%53 : i64)
  ^bb22(%54: i64):  // 2 preds: ^bb20, ^bb33
    %55 = llvm.icmp "slt" %54, %arg1 : i64
    llvm.cond_br %55, ^bb23, ^bb24
  ^bb23:  // pred: ^bb22
    llvm.br ^bb25(%1, %3 : i64, f32)
  ^bb24:  // pred: ^bb22
    %56 = llvm.add %51, %0 : i64
    llvm.br ^bb19(%56 : i64)
  ^bb25(%57: i64, %58: f32):  // 2 preds: ^bb23, ^bb26
    %59 = llvm.icmp "slt" %57, %arg1 : i64
    llvm.cond_br %59, ^bb26, ^bb27
  ^bb26:  // pred: ^bb25
    %60 = llvm.mul %49, %7 : i64
    %61 = llvm.mul %51, %6 : i64
    %62 = llvm.mul %54, %arg1 : i64
    %63 = llvm.add %60, %61 : i64
    %64 = llvm.add %63, %62 : i64
    %65 = llvm.add %64, %57 : i64
    %66 = llvm.getelementptr %arg7[%65] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %67 = llvm.load %66 : !llvm.ptr -> f32
    %68 = llvm.intr.maximum(%58, %67) : (f32, f32) -> f32
    %69 = llvm.add %57, %0 : i64
    llvm.br ^bb25(%69, %68 : i64, f32)
  ^bb27:  // pred: ^bb25
    llvm.br ^bb28(%1, %2 : i64, f32)
  ^bb28(%70: i64, %71: f32):  // 2 preds: ^bb27, ^bb29
    %72 = llvm.icmp "slt" %70, %arg1 : i64
    llvm.cond_br %72, ^bb29, ^bb30
  ^bb29:  // pred: ^bb28
    %73 = llvm.mul %49, %7 : i64
    %74 = llvm.mul %51, %6 : i64
    %75 = llvm.mul %54, %arg1 : i64
    %76 = llvm.add %73, %74 : i64
    %77 = llvm.add %76, %75 : i64
    %78 = llvm.add %77, %70 : i64
    %79 = llvm.getelementptr %arg7[%78] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %80 = llvm.load %79 : !llvm.ptr -> f32
    %81 = llvm.fsub %80, %58 : f32
    %82 = llvm.call @bench_expf(%81) : (f32) -> f32
    %83 = llvm.mul %49, %7 : i64
    %84 = llvm.mul %51, %6 : i64
    %85 = llvm.mul %54, %arg1 : i64
    %86 = llvm.add %83, %84 : i64
    %87 = llvm.add %86, %85 : i64
    %88 = llvm.add %87, %70 : i64
    %89 = llvm.getelementptr %arg8[%88] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %82, %89 : f32, !llvm.ptr
    %90 = llvm.fadd %71, %82 : f32
    %91 = llvm.add %70, %0 : i64
    llvm.br ^bb28(%91, %90 : i64, f32)
  ^bb30:  // pred: ^bb28
    llvm.br ^bb31(%1 : i64)
  ^bb31(%92: i64):  // 2 preds: ^bb30, ^bb32
    %93 = llvm.icmp "slt" %92, %arg1 : i64
    llvm.cond_br %93, ^bb32, ^bb33
  ^bb32:  // pred: ^bb31
    %94 = llvm.mul %49, %7 : i64
    %95 = llvm.mul %51, %6 : i64
    %96 = llvm.mul %54, %arg1 : i64
    %97 = llvm.add %94, %95 : i64
    %98 = llvm.add %97, %96 : i64
    %99 = llvm.add %98, %92 : i64
    %100 = llvm.getelementptr %arg8[%99] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %101 = llvm.load %100 : !llvm.ptr -> f32
    %102 = llvm.fdiv %101, %71 : f32
    %103 = llvm.mul %49, %7 : i64
    %104 = llvm.mul %51, %6 : i64
    %105 = llvm.mul %54, %arg1 : i64
    %106 = llvm.add %103, %104 : i64
    %107 = llvm.add %106, %105 : i64
    %108 = llvm.add %107, %92 : i64
    %109 = llvm.getelementptr %arg8[%108] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %102, %109 : f32, !llvm.ptr
    %110 = llvm.add %92, %0 : i64
    llvm.br ^bb31(%110 : i64)
  ^bb33:  // pred: ^bb31
    %111 = llvm.add %54, %0 : i64
    llvm.br ^bb22(%111 : i64)
  ^bb34(%112: i64):  // 2 preds: ^bb18, ^bb39
    %113 = llvm.icmp "slt" %112, %arg0 : i64
    llvm.cond_br %113, ^bb35, ^bb36
  ^bb35:  // pred: ^bb34
    llvm.br ^bb37(%1 : i64)
  ^bb36:  // pred: ^bb34
    llvm.return
  ^bb37(%114: i64):  // 2 preds: ^bb35, ^bb54
    %115 = llvm.icmp "slt" %114, %arg1 : i64
    llvm.cond_br %115, ^bb38, ^bb39
  ^bb38:  // pred: ^bb37
    llvm.br ^bb40(%1 : i64)
  ^bb39:  // pred: ^bb37
    %116 = llvm.add %112, %0 : i64
    llvm.br ^bb34(%116 : i64)
  ^bb40(%117: i64):  // 2 preds: ^bb38, ^bb45
    %118 = llvm.icmp "slt" %117, %arg2 : i64
    llvm.cond_br %118, ^bb41, ^bb42
  ^bb41:  // pred: ^bb40
    %119 = llvm.mul %117, %arg3 : i64
    llvm.br ^bb43(%1 : i64)
  ^bb42:  // pred: ^bb40
    llvm.br ^bb49(%1, %2 : i64, f32)
  ^bb43(%120: i64):  // 2 preds: ^bb41, ^bb48
    %121 = llvm.icmp "slt" %120, %arg3 : i64
    llvm.cond_br %121, ^bb44, ^bb45
  ^bb44:  // pred: ^bb43
    %122 = llvm.add %119, %120 : i64
    llvm.br ^bb46(%1, %2 : i64, f32)
  ^bb45:  // pred: ^bb43
    %123 = llvm.add %117, %0 : i64
    llvm.br ^bb40(%123 : i64)
  ^bb46(%124: i64, %125: f32):  // 2 preds: ^bb44, ^bb47
    %126 = llvm.icmp "slt" %124, %arg1 : i64
    llvm.cond_br %126, ^bb47, ^bb48
  ^bb47:  // pred: ^bb46
    %127 = llvm.mul %112, %7 : i64
    %128 = llvm.mul %117, %6 : i64
    %129 = llvm.mul %114, %arg1 : i64
    %130 = llvm.add %127, %128 : i64
    %131 = llvm.add %130, %129 : i64
    %132 = llvm.add %131, %124 : i64
    %133 = llvm.getelementptr %arg8[%132] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %134 = llvm.load %133 : !llvm.ptr -> f32
    %135 = llvm.mul %112, %5 : i64
    %136 = llvm.mul %124, %4 : i64
    %137 = llvm.add %135, %136 : i64
    %138 = llvm.add %137, %122 : i64
    %139 = llvm.getelementptr %arg6[%138] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %140 = llvm.load %139 : !llvm.ptr -> f32
    %141 = llvm.fmul %134, %140 : f32
    %142 = llvm.fadd %125, %141 : f32
    %143 = llvm.add %124, %0 : i64
    llvm.br ^bb46(%143, %142 : i64, f32)
  ^bb48:  // pred: ^bb46
    %144 = llvm.mul %112, %5 : i64
    %145 = llvm.mul %114, %4 : i64
    %146 = llvm.add %144, %145 : i64
    %147 = llvm.add %146, %122 : i64
    %148 = llvm.getelementptr %arg9[%147] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %125, %148 : f32, !llvm.ptr
    %149 = llvm.add %120, %0 : i64
    llvm.br ^bb43(%149 : i64)
  ^bb49(%150: i64, %151: f32):  // 2 preds: ^bb42, ^bb53
    %152 = llvm.icmp "slt" %150, %arg2 : i64
    llvm.cond_br %152, ^bb50, ^bb54
  ^bb50:  // pred: ^bb49
    llvm.br ^bb51(%150, %1, %151 : i64, i64, f32)
  ^bb51(%153: i64, %154: i64, %155: f32):  // 2 preds: ^bb50, ^bb52
    %156 = llvm.icmp "slt" %154, %arg3 : i64
    llvm.cond_br %156, ^bb52, ^bb53
  ^bb52:  // pred: ^bb51
    %157 = llvm.mul %153, %arg3 : i64
    %158 = llvm.add %157, %154 : i64
    %159 = llvm.mul %112, %5 : i64
    %160 = llvm.mul %114, %4 : i64
    %161 = llvm.add %159, %160 : i64
    %162 = llvm.add %161, %158 : i64
    %163 = llvm.getelementptr %arg9[%162] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %164 = llvm.load %163 : !llvm.ptr -> f32
    %165 = llvm.mul %112, %5 : i64
    %166 = llvm.mul %114, %4 : i64
    %167 = llvm.add %165, %166 : i64
    %168 = llvm.add %167, %158 : i64
    %169 = llvm.getelementptr %arg10[%168] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %164, %169 : f32, !llvm.ptr
    %170 = llvm.add %154, %0 : i64
    llvm.br ^bb51(%153, %170, %164 : i64, i64, f32)
  ^bb53:  // pred: ^bb51
    %171 = llvm.add %153, %0 : i64
    llvm.br ^bb49(%171, %155 : i64, f32)
  ^bb54:  // pred: ^bb49
    %172 = llvm.add %114, %0 : i64
    llvm.br ^bb37(%172 : i64)
  }
}


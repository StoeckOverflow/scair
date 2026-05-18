module {
  llvm.func @conv2d_dynamic(%arg0: i64, %arg1: i64, %arg2: i64, %arg3: i64, %arg4: i64, %arg5: i64, %arg6: i64, %arg7: i64, %arg8: i64, %arg9: !llvm.ptr, %arg10: !llvm.ptr, %arg11: i64, %arg12: i64, %arg13: i64, %arg14: !llvm.ptr, %arg15: !llvm.ptr, %arg16: i64, %arg17: i64, %arg18: i64, %arg19: !llvm.ptr, %arg20: !llvm.ptr, %arg21: i64, %arg22: i64, %arg23: i64) attributes {llvm.emit_c_interface} {
    %0 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %1 = llvm.insertvalue %arg19, %0[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %2 = llvm.insertvalue %arg20, %1[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %3 = llvm.insertvalue %arg21, %2[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %4 = llvm.insertvalue %arg22, %3[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %5 = llvm.insertvalue %arg23, %4[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %6 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %7 = llvm.insertvalue %arg14, %6[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %8 = llvm.insertvalue %arg15, %7[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %9 = llvm.insertvalue %arg16, %8[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %10 = llvm.insertvalue %arg17, %9[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %11 = llvm.insertvalue %arg18, %10[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %12 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %13 = llvm.insertvalue %arg9, %12[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %14 = llvm.insertvalue %arg10, %13[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %15 = llvm.insertvalue %arg11, %14[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %16 = llvm.insertvalue %arg12, %15[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %17 = llvm.insertvalue %arg13, %16[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %18 = llvm.mlir.constant(0 : index) : i64
    %19 = llvm.mlir.constant(1 : index) : i64
    %20 = llvm.mlir.constant(0.000000e+00 : f32) : f32
    %21 = llvm.mul %arg2, %arg3 : i64
    %22 = llvm.mul %arg1, %21 : i64
    %23 = llvm.mul %arg5, %arg6 : i64
    %24 = llvm.mul %arg1, %23 : i64
    %25 = llvm.mul %arg7, %arg8 : i64
    %26 = llvm.mul %arg4, %25 : i64
    %27 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)>
    %28 = llvm.extractvalue %17[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %29 = llvm.extractvalue %17[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %30 = llvm.insertvalue %28, %27[0] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %31 = llvm.insertvalue %29, %30[1] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %32 = llvm.insertvalue %18, %31[2] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %33 = llvm.insertvalue %arg0, %32[3, 0] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %34 = llvm.insertvalue %22, %33[4, 0] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %35 = llvm.insertvalue %arg1, %34[3, 1] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %36 = llvm.insertvalue %21, %35[4, 1] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %37 = llvm.insertvalue %arg7, %36[3, 2] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %38 = llvm.insertvalue %arg3, %37[4, 2] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %39 = llvm.insertvalue %arg8, %38[3, 3] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %40 = llvm.insertvalue %19, %39[4, 3] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %41 = llvm.insertvalue %arg5, %40[3, 4] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %42 = llvm.insertvalue %arg3, %41[4, 4] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %43 = llvm.insertvalue %arg6, %42[3, 5] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %44 = llvm.insertvalue %19, %43[4, 5] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %45 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %46 = llvm.extractvalue %11[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %47 = llvm.extractvalue %11[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %48 = llvm.insertvalue %46, %45[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %49 = llvm.insertvalue %47, %48[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %50 = llvm.insertvalue %18, %49[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %51 = llvm.insertvalue %arg4, %50[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %52 = llvm.insertvalue %24, %51[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %53 = llvm.insertvalue %arg1, %52[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %54 = llvm.insertvalue %23, %53[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %55 = llvm.insertvalue %arg5, %54[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %56 = llvm.insertvalue %arg6, %55[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %57 = llvm.insertvalue %arg6, %56[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %58 = llvm.insertvalue %19, %57[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %59 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)>
    %60 = llvm.extractvalue %5[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %61 = llvm.extractvalue %5[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %62 = llvm.insertvalue %60, %59[0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %63 = llvm.insertvalue %61, %62[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %64 = llvm.insertvalue %18, %63[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %65 = llvm.insertvalue %arg0, %64[3, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %66 = llvm.insertvalue %26, %65[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %67 = llvm.insertvalue %arg4, %66[3, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %68 = llvm.insertvalue %25, %67[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %69 = llvm.insertvalue %arg7, %68[3, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %70 = llvm.insertvalue %arg8, %69[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %71 = llvm.insertvalue %arg8, %70[3, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %72 = llvm.insertvalue %19, %71[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    llvm.br ^bb1(%18 : i64)
  ^bb1(%73: i64):  // 2 preds: ^bb0, ^bb20
    %74 = llvm.icmp "slt" %73, %arg0 : i64
    llvm.cond_br %74, ^bb2, ^bb21
  ^bb2:  // pred: ^bb1
    llvm.br ^bb3(%18 : i64)
  ^bb3(%75: i64):  // 2 preds: ^bb2, ^bb19
    %76 = llvm.icmp "slt" %75, %arg4 : i64
    llvm.cond_br %76, ^bb4, ^bb20
  ^bb4:  // pred: ^bb3
    llvm.br ^bb5(%18 : i64)
  ^bb5(%77: i64):  // 2 preds: ^bb4, ^bb18
    %78 = llvm.icmp "slt" %77, %arg7 : i64
    llvm.cond_br %78, ^bb6, ^bb19
  ^bb6:  // pred: ^bb5
    llvm.br ^bb7(%18 : i64)
  ^bb7(%79: i64):  // 2 preds: ^bb6, ^bb17
    %80 = llvm.icmp "slt" %79, %arg8 : i64
    llvm.cond_br %80, ^bb8, ^bb18
  ^bb8:  // pred: ^bb7
    llvm.br ^bb9(%18, %20 : i64, f32)
  ^bb9(%81: i64, %82: f32):  // 2 preds: ^bb8, ^bb16
    %83 = llvm.icmp "slt" %81, %arg1 : i64
    llvm.cond_br %83, ^bb10, ^bb17
  ^bb10:  // pred: ^bb9
    llvm.br ^bb11(%18, %82 : i64, f32)
  ^bb11(%84: i64, %85: f32):  // 2 preds: ^bb10, ^bb15
    %86 = llvm.icmp "slt" %84, %arg5 : i64
    llvm.cond_br %86, ^bb12, ^bb16
  ^bb12:  // pred: ^bb11
    llvm.br ^bb13(%18, %85 : i64, f32)
  ^bb13(%87: i64, %88: f32):  // 2 preds: ^bb12, ^bb14
    %89 = llvm.icmp "slt" %87, %arg6 : i64
    llvm.cond_br %89, ^bb14, ^bb15
  ^bb14:  // pred: ^bb13
    %90 = llvm.extractvalue %44[1] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %91 = llvm.extractvalue %44[2] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %92 = llvm.getelementptr %90[%91] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %93 = llvm.extractvalue %44[4, 0] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %94 = llvm.mul %73, %93 overflow<nsw, nuw> : i64
    %95 = llvm.extractvalue %44[4, 1] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %96 = llvm.mul %81, %95 overflow<nsw, nuw> : i64
    %97 = llvm.add %94, %96 overflow<nsw, nuw> : i64
    %98 = llvm.extractvalue %44[4, 2] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %99 = llvm.mul %77, %98 overflow<nsw, nuw> : i64
    %100 = llvm.add %97, %99 overflow<nsw, nuw> : i64
    %101 = llvm.extractvalue %44[4, 3] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %102 = llvm.mul %79, %101 overflow<nsw, nuw> : i64
    %103 = llvm.add %100, %102 overflow<nsw, nuw> : i64
    %104 = llvm.extractvalue %44[4, 4] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %105 = llvm.mul %84, %104 overflow<nsw, nuw> : i64
    %106 = llvm.add %103, %105 overflow<nsw, nuw> : i64
    %107 = llvm.extractvalue %44[4, 5] : !llvm.struct<(ptr, ptr, i64, array<6 x i64>, array<6 x i64>)> 
    %108 = llvm.mul %87, %107 overflow<nsw, nuw> : i64
    %109 = llvm.add %106, %108 overflow<nsw, nuw> : i64
    %110 = llvm.getelementptr inbounds|nuw %92[%109] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %111 = llvm.load %110 : !llvm.ptr -> f32
    %112 = llvm.extractvalue %58[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %113 = llvm.extractvalue %58[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %114 = llvm.getelementptr %112[%113] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %115 = llvm.extractvalue %58[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %116 = llvm.mul %75, %115 overflow<nsw, nuw> : i64
    %117 = llvm.extractvalue %58[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %118 = llvm.mul %81, %117 overflow<nsw, nuw> : i64
    %119 = llvm.add %116, %118 overflow<nsw, nuw> : i64
    %120 = llvm.extractvalue %58[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %121 = llvm.mul %84, %120 overflow<nsw, nuw> : i64
    %122 = llvm.add %119, %121 overflow<nsw, nuw> : i64
    %123 = llvm.extractvalue %58[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %124 = llvm.mul %87, %123 overflow<nsw, nuw> : i64
    %125 = llvm.add %122, %124 overflow<nsw, nuw> : i64
    %126 = llvm.getelementptr inbounds|nuw %114[%125] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %127 = llvm.load %126 : !llvm.ptr -> f32
    %128 = llvm.fmul %111, %127 : f32
    %129 = llvm.fadd %88, %128 : f32
    %130 = llvm.add %87, %19 : i64
    llvm.br ^bb13(%130, %129 : i64, f32)
  ^bb15:  // pred: ^bb13
    %131 = llvm.add %84, %19 : i64
    llvm.br ^bb11(%131, %88 : i64, f32)
  ^bb16:  // pred: ^bb11
    %132 = llvm.add %81, %19 : i64
    llvm.br ^bb9(%132, %85 : i64, f32)
  ^bb17:  // pred: ^bb9
    %133 = llvm.extractvalue %72[1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %134 = llvm.extractvalue %72[2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %135 = llvm.getelementptr %133[%134] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    %136 = llvm.extractvalue %72[4, 0] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %137 = llvm.mul %73, %136 overflow<nsw, nuw> : i64
    %138 = llvm.extractvalue %72[4, 1] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %139 = llvm.mul %75, %138 overflow<nsw, nuw> : i64
    %140 = llvm.add %137, %139 overflow<nsw, nuw> : i64
    %141 = llvm.extractvalue %72[4, 2] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %142 = llvm.mul %77, %141 overflow<nsw, nuw> : i64
    %143 = llvm.add %140, %142 overflow<nsw, nuw> : i64
    %144 = llvm.extractvalue %72[4, 3] : !llvm.struct<(ptr, ptr, i64, array<4 x i64>, array<4 x i64>)> 
    %145 = llvm.mul %79, %144 overflow<nsw, nuw> : i64
    %146 = llvm.add %143, %145 overflow<nsw, nuw> : i64
    %147 = llvm.getelementptr inbounds|nuw %135[%146] : (!llvm.ptr, i64) -> !llvm.ptr, f32
    llvm.store %82, %147 : f32, !llvm.ptr
    %148 = llvm.add %79, %19 : i64
    llvm.br ^bb7(%148 : i64)
  ^bb18:  // pred: ^bb7
    %149 = llvm.add %77, %19 : i64
    llvm.br ^bb5(%149 : i64)
  ^bb19:  // pred: ^bb5
    %150 = llvm.add %75, %19 : i64
    llvm.br ^bb3(%150 : i64)
  ^bb20:  // pred: ^bb3
    %151 = llvm.add %73, %19 : i64
    llvm.br ^bb1(%151 : i64)
  ^bb21:  // pred: ^bb1
    llvm.return
  }
  llvm.func @_mlir_ciface_conv2d_dynamic(%arg0: i64, %arg1: i64, %arg2: i64, %arg3: i64, %arg4: i64, %arg5: i64, %arg6: i64, %arg7: i64, %arg8: i64, %arg9: !llvm.ptr, %arg10: !llvm.ptr, %arg11: !llvm.ptr) attributes {llvm.emit_c_interface} {
    %0 = llvm.load %arg9 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %1 = llvm.extractvalue %0[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %2 = llvm.extractvalue %0[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %3 = llvm.extractvalue %0[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %4 = llvm.extractvalue %0[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %5 = llvm.extractvalue %0[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %6 = llvm.load %arg10 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %7 = llvm.extractvalue %6[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %8 = llvm.extractvalue %6[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %9 = llvm.extractvalue %6[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %10 = llvm.extractvalue %6[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %11 = llvm.extractvalue %6[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %12 = llvm.load %arg11 : !llvm.ptr -> !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %13 = llvm.extractvalue %12[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %14 = llvm.extractvalue %12[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %15 = llvm.extractvalue %12[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %16 = llvm.extractvalue %12[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %17 = llvm.extractvalue %12[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    llvm.call @conv2d_dynamic(%arg0, %arg1, %arg2, %arg3, %arg4, %arg5, %arg6, %arg7, %arg8, %1, %2, %3, %4, %5, %7, %8, %9, %10, %11, %13, %14, %15, %16, %17) : (i64, i64, i64, i64, i64, i64, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64) -> ()
    llvm.return
  }
}


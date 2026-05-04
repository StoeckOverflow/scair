module {
  llvm.func @blocked_pack(%arg0: i64, %arg1: i64, %arg2: i64, %arg3: i64, %arg4: !llvm.ptr, %arg5: !llvm.ptr, %arg6: i64, %arg7: i64, %arg8: i64, %arg9: !llvm.ptr, %arg10: !llvm.ptr, %arg11: i64, %arg12: i64, %arg13: i64) attributes {llvm.emit_c_interface} {
    %0 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %1 = llvm.insertvalue %arg9, %0[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %2 = llvm.insertvalue %arg10, %1[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %3 = llvm.insertvalue %arg11, %2[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %4 = llvm.insertvalue %arg12, %3[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %5 = llvm.insertvalue %arg13, %4[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %6 = llvm.mlir.poison : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)>
    %7 = llvm.insertvalue %arg4, %6[0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %8 = llvm.insertvalue %arg5, %7[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %9 = llvm.insertvalue %arg6, %8[2] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %10 = llvm.insertvalue %arg7, %9[3, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %11 = llvm.insertvalue %arg8, %10[4, 0] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %12 = llvm.mlir.constant(0 : index) : i64
    %13 = llvm.mlir.constant(1 : index) : i64
    %14 = llvm.mul %arg0, %arg2 : i64
    %15 = llvm.mul %arg1, %arg3 : i64
    llvm.br ^bb1(%12 : i64)
  ^bb1(%16: i64):  // 2 preds: ^bb0, ^bb11
    %17 = llvm.icmp "slt" %16, %arg0 : i64
    llvm.cond_br %17, ^bb2, ^bb12
  ^bb2:  // pred: ^bb1
    %18 = llvm.mul %16, %arg2 : i64
    %19 = llvm.sub %14, %18 : i64
    %20 = llvm.intr.smin(%arg2, %19) : (i64, i64) -> i64
    llvm.br ^bb3(%12 : i64)
  ^bb3(%21: i64):  // 2 preds: ^bb2, ^bb10
    %22 = llvm.icmp "slt" %21, %arg1 : i64
    llvm.cond_br %22, ^bb4, ^bb11
  ^bb4:  // pred: ^bb3
    %23 = llvm.mul %21, %arg3 : i64
    %24 = llvm.sub %15, %23 : i64
    %25 = llvm.intr.smin(%arg3, %24) : (i64, i64) -> i64
    llvm.br ^bb5(%12 : i64)
  ^bb5(%26: i64):  // 2 preds: ^bb4, ^bb9
    %27 = llvm.icmp "slt" %26, %20 : i64
    llvm.cond_br %27, ^bb6, ^bb10
  ^bb6:  // pred: ^bb5
    %28 = llvm.add %18, %26 : i64
    llvm.br ^bb7(%12 : i64)
  ^bb7(%29: i64):  // 2 preds: ^bb6, ^bb8
    %30 = llvm.icmp "slt" %29, %25 : i64
    llvm.cond_br %30, ^bb8, ^bb9
  ^bb8:  // pred: ^bb7
    %31 = llvm.add %23, %29 : i64
    %32 = llvm.mul %28, %15 : i64
    %33 = llvm.add %32, %31 : i64
    %34 = llvm.mul %16, %arg1 : i64
    %35 = llvm.add %34, %21 : i64
    %36 = llvm.mul %35, %arg2 : i64
    %37 = llvm.add %36, %26 : i64
    %38 = llvm.mul %37, %arg3 : i64
    %39 = llvm.add %38, %29 : i64
    %40 = llvm.extractvalue %11[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %41 = llvm.getelementptr inbounds|nuw %40[%33] : (!llvm.ptr, i64) -> !llvm.ptr, i64
    %42 = llvm.load %41 : !llvm.ptr -> i64
    %43 = llvm.extractvalue %5[1] : !llvm.struct<(ptr, ptr, i64, array<1 x i64>, array<1 x i64>)> 
    %44 = llvm.getelementptr inbounds|nuw %43[%39] : (!llvm.ptr, i64) -> !llvm.ptr, i64
    llvm.store %42, %44 : i64, !llvm.ptr
    %45 = llvm.add %29, %13 : i64
    llvm.br ^bb7(%45 : i64)
  ^bb9:  // pred: ^bb7
    %46 = llvm.add %26, %13 : i64
    llvm.br ^bb5(%46 : i64)
  ^bb10:  // pred: ^bb5
    %47 = llvm.add %21, %13 : i64
    llvm.br ^bb3(%47 : i64)
  ^bb11:  // pred: ^bb3
    %48 = llvm.add %16, %13 : i64
    llvm.br ^bb1(%48 : i64)
  ^bb12:  // pred: ^bb1
    llvm.return
  }
  llvm.func @_mlir_ciface_blocked_pack(%arg0: i64, %arg1: i64, %arg2: i64, %arg3: i64, %arg4: !llvm.ptr, %arg5: !llvm.ptr) attributes {llvm.emit_c_interface} {
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
    llvm.call @blocked_pack(%arg0, %arg1, %arg2, %arg3, %1, %2, %3, %4, %5, %7, %8, %9, %10, %11) : (i64, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64, !llvm.ptr, !llvm.ptr, i64, i64, i64) -> ()
    llvm.return
  }
}


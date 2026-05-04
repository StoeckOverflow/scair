builtin.module {
  llvm.func @broadcast_affine_2d(%0: i64, %1: i64, %2: !llvm.ptr, %3: !llvm.ptr, %4: !llvm.ptr, %5: !llvm.ptr) {
%6 = llvm.load %2 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%7 = llvm.load %3 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%8 = llvm.load %4 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%9 = llvm.load %5 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%10 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%11 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%12 = "llvm.mul"(%0, %1) : (i64, i64) -> i64
%13 = llvm.extractvalue %6[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%14 = llvm.extractvalue %6[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%15 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%16 = llvm.insertvalue %13, %15[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%17 = llvm.insertvalue %14, %16[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%18 = llvm.insertvalue %10, %17[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%19 = llvm.insertvalue %0, %18[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%20 = llvm.insertvalue %1, %19[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%21 = llvm.insertvalue %1, %20[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%22 = llvm.insertvalue %11, %21[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%23 = llvm.extractvalue %7[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%24 = llvm.extractvalue %7[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%25 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%26 = llvm.insertvalue %23, %25[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%27 = llvm.insertvalue %24, %26[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%28 = llvm.insertvalue %10, %27[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%29 = llvm.insertvalue %1, %28[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%30 = llvm.insertvalue %11, %29[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%31 = llvm.extractvalue %8[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%32 = llvm.extractvalue %8[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%33 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%34 = llvm.insertvalue %31, %33[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%35 = llvm.insertvalue %32, %34[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%36 = llvm.insertvalue %10, %35[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%37 = llvm.insertvalue %1, %36[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%38 = llvm.insertvalue %11, %37[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%39 = llvm.extractvalue %9[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%40 = llvm.extractvalue %9[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%41 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%42 = llvm.insertvalue %39, %41[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%43 = llvm.insertvalue %40, %42[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%44 = llvm.insertvalue %10, %43[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%45 = llvm.insertvalue %0, %44[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%46 = llvm.insertvalue %1, %45[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%47 = llvm.insertvalue %1, %46[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%48 = llvm.insertvalue %11, %47[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
"llvm.br"(%10)[^bb0] : (i64) -> ()
  ^bb0(%49: i64):
    %50 = "llvm.icmp"(%49, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%50)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%10)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%51: i64):
    %52 = "llvm.icmp"(%51, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%52)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    %53 = llvm.extractvalue %22[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %54 = llvm.extractvalue %22[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %55 = "llvm.mul"(%49, %54) : (i64, i64) -> i64
    %56 = llvm.extractvalue %22[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %57 = "llvm.mul"(%51, %56) : (i64, i64) -> i64
    %58 = "llvm.add"(%55, %57) : (i64, i64) -> i64
    %59 = "llvm.getelementptr"(%53, %58) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    %60 = llvm.load %59 : !llvm.ptr -> i64
    %61 = llvm.extractvalue %30[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
    %62 = llvm.extractvalue %30[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
    %63 = "llvm.mul"(%51, %62) : (i64, i64) -> i64
    %64 = "llvm.getelementptr"(%61, %63) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    %65 = llvm.load %64 : !llvm.ptr -> i64
    %66 = llvm.extractvalue %38[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
    %67 = llvm.extractvalue %38[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
    %68 = "llvm.mul"(%51, %67) : (i64, i64) -> i64
    %69 = "llvm.getelementptr"(%66, %68) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    %70 = llvm.load %69 : !llvm.ptr -> i64
    %71 = "llvm.mul"(%60, %65) : (i64, i64) -> i64
    %72 = "llvm.add"(%71, %70) : (i64, i64) -> i64
    %73 = llvm.extractvalue %48[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %74 = llvm.extractvalue %48[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %75 = "llvm.mul"(%49, %74) : (i64, i64) -> i64
    %76 = llvm.extractvalue %48[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %77 = "llvm.mul"(%51, %76) : (i64, i64) -> i64
    %78 = "llvm.add"(%75, %77) : (i64, i64) -> i64
    %79 = "llvm.getelementptr"(%73, %78) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%72, %79) : (i64, !llvm.ptr) -> ()
    %80 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %81 = "llvm.add"(%51, %80) : (i64, i64) -> i64
    "llvm.br"(%81)[^bb3] : (i64) -> ()
  ^bb5:
    %82 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %83 = "llvm.add"(%49, %82) : (i64, i64) -> i64
    "llvm.br"(%83)[^bb0] : (i64) -> ()
  }
}

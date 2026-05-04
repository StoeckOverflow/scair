builtin.module {
  llvm.func @blocked_pack(%0: i64, %1: i64, %2: i64, %3: i64, %4: !llvm.ptr, %5: !llvm.ptr) {
%6 = llvm.load %4 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%7 = llvm.load %5 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%8 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%9 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%10 = "llvm.mul"(%0, %2) : (i64, i64) -> i64
%11 = "llvm.mul"(%1, %3) : (i64, i64) -> i64
%12 = "llvm.mul"(%2, %11) : (i64, i64) -> i64
%13 = "llvm.mul"(%3, %9) : (i64, i64) -> i64
%14 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
%15 = "llvm.mul"(%1, %14) : (i64, i64) -> i64
%16 = llvm.extractvalue %6[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%17 = llvm.extractvalue %6[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%18 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%19 = llvm.insertvalue %16, %18[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%20 = llvm.insertvalue %17, %19[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%21 = llvm.insertvalue %8, %20[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%22 = llvm.insertvalue %0, %21[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%23 = llvm.insertvalue %1, %22[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%24 = llvm.insertvalue %2, %23[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%25 = llvm.insertvalue %3, %24[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%26 = llvm.insertvalue %12, %25[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%27 = llvm.insertvalue %3, %26[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%28 = llvm.insertvalue %11, %27[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%29 = llvm.insertvalue %9, %28[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%30 = llvm.extractvalue %7[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%31 = llvm.extractvalue %7[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%32 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%33 = llvm.insertvalue %30, %32[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%34 = llvm.insertvalue %31, %33[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%35 = llvm.insertvalue %8, %34[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%36 = llvm.insertvalue %0, %35[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%37 = llvm.insertvalue %1, %36[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%38 = llvm.insertvalue %2, %37[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%39 = llvm.insertvalue %3, %38[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%40 = llvm.insertvalue %15, %39[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%41 = llvm.insertvalue %14, %40[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%42 = llvm.insertvalue %13, %41[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%43 = llvm.insertvalue %9, %42[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
"llvm.br"(%8)[^bb0] : (i64) -> ()
  ^bb0(%44: i64):
    %45 = "llvm.icmp"(%44, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%45)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%8)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%46: i64):
    %47 = "llvm.icmp"(%46, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%47)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    "llvm.br"(%8)[^bb6] : (i64) -> ()
  ^bb5:
    %48 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %49 = "llvm.add"(%44, %48) : (i64, i64) -> i64
    "llvm.br"(%49)[^bb0] : (i64) -> ()
  ^bb6(%50: i64):
    %51 = "llvm.icmp"(%50, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%51)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    "llvm.br"(%8)[^bb9] : (i64) -> ()
  ^bb8:
    %52 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %53 = "llvm.add"(%46, %52) : (i64, i64) -> i64
    "llvm.br"(%53)[^bb3] : (i64) -> ()
  ^bb9(%54: i64):
    %55 = "llvm.icmp"(%54, %3) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%55)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    %56 = llvm.extractvalue %29[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %57 = llvm.extractvalue %29[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %58 = "llvm.mul"(%44, %57) : (i64, i64) -> i64
    %59 = llvm.extractvalue %29[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %60 = "llvm.mul"(%46, %59) : (i64, i64) -> i64
    %61 = llvm.extractvalue %29[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %62 = "llvm.mul"(%50, %61) : (i64, i64) -> i64
    %63 = llvm.extractvalue %29[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %64 = "llvm.mul"(%54, %63) : (i64, i64) -> i64
    %65 = "llvm.add"(%58, %60) : (i64, i64) -> i64
    %66 = "llvm.add"(%65, %62) : (i64, i64) -> i64
    %67 = "llvm.add"(%66, %64) : (i64, i64) -> i64
    %68 = "llvm.getelementptr"(%56, %67) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    %69 = llvm.load %68 : !llvm.ptr -> i64
    %70 = llvm.extractvalue %43[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %71 = llvm.extractvalue %43[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %72 = "llvm.mul"(%44, %71) : (i64, i64) -> i64
    %73 = llvm.extractvalue %43[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %74 = "llvm.mul"(%46, %73) : (i64, i64) -> i64
    %75 = llvm.extractvalue %43[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %76 = "llvm.mul"(%50, %75) : (i64, i64) -> i64
    %77 = llvm.extractvalue %43[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %78 = "llvm.mul"(%54, %77) : (i64, i64) -> i64
    %79 = "llvm.add"(%72, %74) : (i64, i64) -> i64
    %80 = "llvm.add"(%79, %76) : (i64, i64) -> i64
    %81 = "llvm.add"(%80, %78) : (i64, i64) -> i64
    %82 = "llvm.getelementptr"(%70, %81) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%69, %82) : (i64, !llvm.ptr) -> ()
    %83 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %84 = "llvm.add"(%54, %83) : (i64, i64) -> i64
    "llvm.br"(%84)[^bb9] : (i64) -> ()
  ^bb11:
    %85 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %86 = "llvm.add"(%50, %85) : (i64, i64) -> i64
    "llvm.br"(%86)[^bb6] : (i64) -> ()
  }
}

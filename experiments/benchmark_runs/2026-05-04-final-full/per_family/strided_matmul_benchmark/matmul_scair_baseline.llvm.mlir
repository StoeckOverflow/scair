builtin.module {
  llvm.func @matmul_strided(%0: i64, %1: i64, %2: i64, %3: i64, %4: i64, %5: i64, %6: i64, %7: i64, %8: i64, %9: !llvm.ptr, %10: !llvm.ptr, %11: !llvm.ptr) {
%12 = llvm.load %9 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%13 = llvm.load %10 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%14 = llvm.load %11 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%15 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%16 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%17 = llvm.extractvalue %12[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%18 = llvm.extractvalue %12[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%19 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%20 = llvm.insertvalue %17, %19[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%21 = llvm.insertvalue %18, %20[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%22 = llvm.insertvalue %15, %21[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%23 = llvm.insertvalue %0, %22[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%24 = llvm.insertvalue %2, %23[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%25 = llvm.insertvalue %3, %24[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%26 = llvm.insertvalue %4, %25[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%27 = llvm.extractvalue %13[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%28 = llvm.extractvalue %13[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%29 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%30 = llvm.insertvalue %27, %29[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%31 = llvm.insertvalue %28, %30[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%32 = llvm.insertvalue %15, %31[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%33 = llvm.insertvalue %2, %32[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%34 = llvm.insertvalue %1, %33[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%35 = llvm.insertvalue %5, %34[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%36 = llvm.insertvalue %6, %35[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%37 = llvm.extractvalue %14[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%38 = llvm.extractvalue %14[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%39 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%40 = llvm.insertvalue %37, %39[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%41 = llvm.insertvalue %38, %40[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%42 = llvm.insertvalue %15, %41[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%43 = llvm.insertvalue %0, %42[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%44 = llvm.insertvalue %1, %43[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%45 = llvm.insertvalue %7, %44[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%46 = llvm.insertvalue %8, %45[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
"llvm.br"(%15)[^bb0] : (i64) -> ()
  ^bb0(%47: i64):
    %48 = "llvm.icmp"(%47, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%48)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%15)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%49: i64):
    %50 = "llvm.icmp"(%49, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%50)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    "llvm.br"(%15, %16)[^bb6] : (i64, f32) -> ()
  ^bb5:
    %51 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %52 = "llvm.add"(%47, %51) : (i64, i64) -> i64
    "llvm.br"(%52)[^bb0] : (i64) -> ()
  ^bb6(%53: i64, %54: f32):
    %55 = "llvm.icmp"(%53, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%55)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    %56 = llvm.extractvalue %26[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %57 = llvm.extractvalue %26[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %58 = "llvm.mul"(%47, %57) : (i64, i64) -> i64
    %59 = llvm.extractvalue %26[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %60 = "llvm.mul"(%53, %59) : (i64, i64) -> i64
    %61 = "llvm.add"(%58, %60) : (i64, i64) -> i64
    %62 = "llvm.getelementptr"(%56, %61) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %63 = llvm.load %62 : !llvm.ptr -> f32
    %64 = llvm.extractvalue %36[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %65 = llvm.extractvalue %36[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %66 = "llvm.mul"(%53, %65) : (i64, i64) -> i64
    %67 = llvm.extractvalue %36[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %68 = "llvm.mul"(%49, %67) : (i64, i64) -> i64
    %69 = "llvm.add"(%66, %68) : (i64, i64) -> i64
    %70 = "llvm.getelementptr"(%64, %69) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %71 = llvm.load %70 : !llvm.ptr -> f32
    %72 = "llvm.fmul"(%63, %71) : (f32, f32) -> f32
    %73 = "llvm.fadd"(%54, %72) : (f32, f32) -> f32
    %74 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %75 = "llvm.add"(%53, %74) : (i64, i64) -> i64
    "llvm.br"(%75, %73)[^bb6] : (i64, f32) -> ()
  ^bb8:
    %76 = llvm.extractvalue %46[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %77 = llvm.extractvalue %46[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %78 = "llvm.mul"(%47, %77) : (i64, i64) -> i64
    %79 = llvm.extractvalue %46[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %80 = "llvm.mul"(%49, %79) : (i64, i64) -> i64
    %81 = "llvm.add"(%78, %80) : (i64, i64) -> i64
    %82 = "llvm.getelementptr"(%76, %81) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%54, %82) : (f32, !llvm.ptr) -> ()
    %83 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %84 = "llvm.add"(%49, %83) : (i64, i64) -> i64
    "llvm.br"(%84)[^bb3] : (i64) -> ()
  }
}

builtin.module {
  llvm.func @matmul_tiling(%0: i64, %1: i64, %2: i64, %3: i64, %4: !llvm.ptr, %5: !llvm.ptr, %6: !llvm.ptr) {
%7 = llvm.load %4 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%8 = llvm.load %5 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%9 = llvm.load %6 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%10 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%11 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%12 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%13 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
%14 = llvm.extractvalue %7[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%15 = llvm.extractvalue %7[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%16 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%17 = llvm.insertvalue %14, %16[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%18 = llvm.insertvalue %15, %17[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%19 = llvm.insertvalue %10, %18[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%20 = llvm.insertvalue %0, %19[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%21 = llvm.insertvalue %13, %20[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%22 = llvm.insertvalue %13, %21[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%23 = llvm.insertvalue %11, %22[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%24 = llvm.extractvalue %8[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%25 = llvm.extractvalue %8[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%26 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%27 = llvm.insertvalue %24, %26[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%28 = llvm.insertvalue %25, %27[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%29 = llvm.insertvalue %10, %28[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%30 = llvm.insertvalue %13, %29[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%31 = llvm.insertvalue %1, %30[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%32 = llvm.insertvalue %1, %31[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%33 = llvm.insertvalue %11, %32[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%34 = llvm.extractvalue %9[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%35 = llvm.extractvalue %9[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%36 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%37 = llvm.insertvalue %34, %36[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%38 = llvm.insertvalue %35, %37[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%39 = llvm.insertvalue %10, %38[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%40 = llvm.insertvalue %0, %39[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%41 = llvm.insertvalue %1, %40[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%42 = llvm.insertvalue %1, %41[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%43 = llvm.insertvalue %11, %42[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
"llvm.br"(%10)[^bb0] : (i64) -> ()
  ^bb0(%44: i64):
    %45 = "llvm.icmp"(%44, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%45)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%10)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%46: i64):
    %47 = "llvm.icmp"(%46, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%47)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    "llvm.br"(%10, %12)[^bb6] : (i64, f32) -> ()
  ^bb5:
    %48 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %49 = "llvm.add"(%44, %48) : (i64, i64) -> i64
    "llvm.br"(%49)[^bb0] : (i64) -> ()
  ^bb6(%50: i64, %51: f32):
    %52 = "llvm.icmp"(%50, %13) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%52)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    %53 = llvm.extractvalue %23[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %54 = llvm.extractvalue %23[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %55 = "llvm.mul"(%44, %54) : (i64, i64) -> i64
    %56 = llvm.extractvalue %23[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %57 = "llvm.mul"(%50, %56) : (i64, i64) -> i64
    %58 = "llvm.add"(%55, %57) : (i64, i64) -> i64
    %59 = "llvm.getelementptr"(%53, %58) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %60 = llvm.load %59 : !llvm.ptr -> f32
    %61 = llvm.extractvalue %33[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %62 = llvm.extractvalue %33[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %63 = "llvm.mul"(%50, %62) : (i64, i64) -> i64
    %64 = llvm.extractvalue %33[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %65 = "llvm.mul"(%46, %64) : (i64, i64) -> i64
    %66 = "llvm.add"(%63, %65) : (i64, i64) -> i64
    %67 = "llvm.getelementptr"(%61, %66) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %68 = llvm.load %67 : !llvm.ptr -> f32
    %69 = "llvm.fmul"(%60, %68) : (f32, f32) -> f32
    %70 = "llvm.fadd"(%51, %69) : (f32, f32) -> f32
    %71 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %72 = "llvm.add"(%50, %71) : (i64, i64) -> i64
    "llvm.br"(%72, %70)[^bb6] : (i64, f32) -> ()
  ^bb8:
    %73 = llvm.extractvalue %43[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %74 = llvm.extractvalue %43[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %75 = "llvm.mul"(%44, %74) : (i64, i64) -> i64
    %76 = llvm.extractvalue %43[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %77 = "llvm.mul"(%46, %76) : (i64, i64) -> i64
    %78 = "llvm.add"(%75, %77) : (i64, i64) -> i64
    %79 = "llvm.getelementptr"(%73, %78) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%51, %79) : (f32, !llvm.ptr) -> ()
    %80 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %81 = "llvm.add"(%46, %80) : (i64, i64) -> i64
    "llvm.br"(%81)[^bb3] : (i64) -> ()
  }
}

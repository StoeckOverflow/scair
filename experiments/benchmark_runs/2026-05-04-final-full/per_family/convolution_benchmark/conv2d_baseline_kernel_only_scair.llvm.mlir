builtin.module {
  llvm.func @conv2d_dynamic(%0: i64, %1: i64, %2: i64, %3: i64, %4: i64, %5: i64, %6: i64, %7: i64, %8: i64, %9: !llvm.ptr, %10: !llvm.ptr, %11: !llvm.ptr) {
%12 = llvm.load %9 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%13 = llvm.load %10 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%14 = llvm.load %11 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%15 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%16 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%17 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%18 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
%19 = "llvm.mul"(%1, %18) : (i64, i64) -> i64
%20 = "llvm.mul"(%5, %6) : (i64, i64) -> i64
%21 = "llvm.mul"(%1, %20) : (i64, i64) -> i64
%22 = "llvm.mul"(%7, %8) : (i64, i64) -> i64
%23 = "llvm.mul"(%4, %22) : (i64, i64) -> i64
%24 = llvm.extractvalue %12[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%25 = llvm.extractvalue %12[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%26 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%27 = llvm.insertvalue %24, %26[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%28 = llvm.insertvalue %25, %27[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%29 = llvm.insertvalue %15, %28[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%30 = llvm.insertvalue %0, %29[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%31 = llvm.insertvalue %1, %30[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%32 = llvm.insertvalue %7, %31[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%33 = llvm.insertvalue %8, %32[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%34 = llvm.insertvalue %5, %33[3, 4] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%35 = llvm.insertvalue %6, %34[3, 5] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%36 = llvm.insertvalue %19, %35[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%37 = llvm.insertvalue %18, %36[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%38 = llvm.insertvalue %3, %37[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%39 = llvm.insertvalue %16, %38[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%40 = llvm.insertvalue %3, %39[4, 4] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%41 = llvm.insertvalue %16, %40[4, 5] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
%42 = llvm.extractvalue %13[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%43 = llvm.extractvalue %13[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%44 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%45 = llvm.insertvalue %42, %44[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%46 = llvm.insertvalue %43, %45[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%47 = llvm.insertvalue %15, %46[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%48 = llvm.insertvalue %4, %47[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%49 = llvm.insertvalue %1, %48[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%50 = llvm.insertvalue %5, %49[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%51 = llvm.insertvalue %6, %50[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%52 = llvm.insertvalue %21, %51[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%53 = llvm.insertvalue %20, %52[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%54 = llvm.insertvalue %6, %53[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%55 = llvm.insertvalue %16, %54[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%56 = llvm.extractvalue %14[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%57 = llvm.extractvalue %14[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%58 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%59 = llvm.insertvalue %56, %58[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%60 = llvm.insertvalue %57, %59[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%61 = llvm.insertvalue %15, %60[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%62 = llvm.insertvalue %0, %61[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%63 = llvm.insertvalue %4, %62[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%64 = llvm.insertvalue %7, %63[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%65 = llvm.insertvalue %8, %64[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%66 = llvm.insertvalue %23, %65[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%67 = llvm.insertvalue %22, %66[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%68 = llvm.insertvalue %8, %67[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%69 = llvm.insertvalue %16, %68[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
"llvm.br"(%15)[^bb0] : (i64) -> ()
  ^bb0(%70: i64):
    %71 = "llvm.icmp"(%70, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%71)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%15)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%72: i64):
    %73 = "llvm.icmp"(%72, %4) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%73)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    "llvm.br"(%15)[^bb6] : (i64) -> ()
  ^bb5:
    %74 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %75 = "llvm.add"(%70, %74) : (i64, i64) -> i64
    "llvm.br"(%75)[^bb0] : (i64) -> ()
  ^bb6(%76: i64):
    %77 = "llvm.icmp"(%76, %7) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%77)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    "llvm.br"(%15)[^bb9] : (i64) -> ()
  ^bb8:
    %78 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %79 = "llvm.add"(%72, %78) : (i64, i64) -> i64
    "llvm.br"(%79)[^bb3] : (i64) -> ()
  ^bb9(%80: i64):
    %81 = "llvm.icmp"(%80, %8) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%81)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    "llvm.br"(%15, %17)[^bb12] : (i64, f32) -> ()
  ^bb11:
    %82 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %83 = "llvm.add"(%76, %82) : (i64, i64) -> i64
    "llvm.br"(%83)[^bb6] : (i64) -> ()
  ^bb12(%84: i64, %85: f32):
    %86 = "llvm.icmp"(%84, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%86)[^bb13, ^bb14] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb13:
    "llvm.br"(%15, %85)[^bb15] : (i64, f32) -> ()
  ^bb15(%87: i64, %88: f32):
    %89 = "llvm.icmp"(%87, %5) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%89)[^bb16, ^bb17] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb16:
    "llvm.br"(%15, %88)[^bb18] : (i64, f32) -> ()
  ^bb17:
    %90 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %91 = "llvm.add"(%84, %90) : (i64, i64) -> i64
    "llvm.br"(%91, %88)[^bb12] : (i64, f32) -> ()
  ^bb14:
    %92 = llvm.extractvalue %69[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %93 = llvm.extractvalue %69[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %94 = "llvm.mul"(%70, %93) : (i64, i64) -> i64
    %95 = llvm.extractvalue %69[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %96 = "llvm.mul"(%72, %95) : (i64, i64) -> i64
    %97 = llvm.extractvalue %69[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %98 = "llvm.mul"(%76, %97) : (i64, i64) -> i64
    %99 = llvm.extractvalue %69[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %100 = "llvm.mul"(%80, %99) : (i64, i64) -> i64
    %101 = "llvm.add"(%94, %96) : (i64, i64) -> i64
    %102 = "llvm.add"(%101, %98) : (i64, i64) -> i64
    %103 = "llvm.add"(%102, %100) : (i64, i64) -> i64
    %104 = "llvm.getelementptr"(%92, %103) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%85, %104) : (f32, !llvm.ptr) -> ()
    %105 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %106 = "llvm.add"(%80, %105) : (i64, i64) -> i64
    "llvm.br"(%106)[^bb9] : (i64) -> ()
  ^bb18(%107: i64, %108: f32):
    %109 = "llvm.icmp"(%107, %6) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%109)[^bb19, ^bb20] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb19:
    %110 = llvm.extractvalue %41[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
    %111 = llvm.extractvalue %41[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
    %112 = "llvm.mul"(%70, %111) : (i64, i64) -> i64
    %113 = llvm.extractvalue %41[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
    %114 = "llvm.mul"(%84, %113) : (i64, i64) -> i64
    %115 = llvm.extractvalue %41[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
    %116 = "llvm.mul"(%76, %115) : (i64, i64) -> i64
    %117 = llvm.extractvalue %41[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
    %118 = "llvm.mul"(%80, %117) : (i64, i64) -> i64
    %119 = llvm.extractvalue %41[4, 4] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
    %120 = "llvm.mul"(%87, %119) : (i64, i64) -> i64
    %121 = llvm.extractvalue %41[4, 5] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<6 x i64>, !llvm.array<6 x i64>)>
    %122 = "llvm.mul"(%107, %121) : (i64, i64) -> i64
    %123 = "llvm.add"(%112, %114) : (i64, i64) -> i64
    %124 = "llvm.add"(%123, %116) : (i64, i64) -> i64
    %125 = "llvm.add"(%124, %118) : (i64, i64) -> i64
    %126 = "llvm.add"(%125, %120) : (i64, i64) -> i64
    %127 = "llvm.add"(%126, %122) : (i64, i64) -> i64
    %128 = "llvm.getelementptr"(%110, %127) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %129 = llvm.load %128 : !llvm.ptr -> f32
    %130 = llvm.extractvalue %55[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %131 = llvm.extractvalue %55[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %132 = "llvm.mul"(%72, %131) : (i64, i64) -> i64
    %133 = llvm.extractvalue %55[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %134 = "llvm.mul"(%84, %133) : (i64, i64) -> i64
    %135 = llvm.extractvalue %55[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %136 = "llvm.mul"(%87, %135) : (i64, i64) -> i64
    %137 = llvm.extractvalue %55[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %138 = "llvm.mul"(%107, %137) : (i64, i64) -> i64
    %139 = "llvm.add"(%132, %134) : (i64, i64) -> i64
    %140 = "llvm.add"(%139, %136) : (i64, i64) -> i64
    %141 = "llvm.add"(%140, %138) : (i64, i64) -> i64
    %142 = "llvm.getelementptr"(%130, %141) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %143 = llvm.load %142 : !llvm.ptr -> f32
    %144 = "llvm.fmul"(%129, %143) : (f32, f32) -> f32
    %145 = "llvm.fadd"(%108, %144) : (f32, f32) -> f32
    %146 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %147 = "llvm.add"(%107, %146) : (i64, i64) -> i64
    "llvm.br"(%147, %145)[^bb18] : (i64, f32) -> ()
  ^bb20:
    %148 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %149 = "llvm.add"(%87, %148) : (i64, i64) -> i64
    "llvm.br"(%149, %108)[^bb15] : (i64, f32) -> ()
  }
}

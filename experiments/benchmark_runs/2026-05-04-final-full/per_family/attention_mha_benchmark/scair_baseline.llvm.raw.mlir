builtin.module {
  llvm.func @bench_expf(f32) -> f32
  llvm.func @bench_inv_sqrt_index(i64) -> f32
  llvm.func @attention_mha(%0: i64, %1: i64, %2: i64, %3: i64, %4: !llvm.ptr, %5: !llvm.ptr, %6: !llvm.ptr, %7: !llvm.ptr, %8: !llvm.ptr, %9: !llvm.ptr, %10: !llvm.ptr) {
%11 = llvm.load %4 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%12 = llvm.load %5 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%13 = llvm.load %6 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%14 = llvm.load %7 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%15 = llvm.load %8 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%16 = llvm.load %9 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%17 = llvm.load %10 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%18 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%19 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%20 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%21 = "llvm.mlir.constant"() <{value = -3.40282347E38 : f32}> : () -> f32
%22 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
%23 = "llvm.mul"(%1, %22) : (i64, i64) -> i64
%24 = "llvm.mul"(%1, %1) : (i64, i64) -> i64
%25 = "llvm.mul"(%2, %24) : (i64, i64) -> i64
%26 = llvm.extractvalue %11[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%27 = llvm.extractvalue %11[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%28 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%29 = llvm.insertvalue %26, %28[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%30 = llvm.insertvalue %27, %29[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%31 = llvm.insertvalue %18, %30[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%32 = llvm.insertvalue %0, %31[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%33 = llvm.insertvalue %1, %32[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%34 = llvm.insertvalue %2, %33[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%35 = llvm.insertvalue %3, %34[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%36 = llvm.insertvalue %23, %35[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%37 = llvm.insertvalue %22, %36[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%38 = llvm.insertvalue %3, %37[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%39 = llvm.insertvalue %19, %38[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%40 = llvm.extractvalue %12[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%41 = llvm.extractvalue %12[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%42 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%43 = llvm.insertvalue %40, %42[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%44 = llvm.insertvalue %41, %43[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%45 = llvm.insertvalue %18, %44[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%46 = llvm.insertvalue %0, %45[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%47 = llvm.insertvalue %1, %46[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%48 = llvm.insertvalue %2, %47[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%49 = llvm.insertvalue %3, %48[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%50 = llvm.insertvalue %23, %49[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%51 = llvm.insertvalue %22, %50[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%52 = llvm.insertvalue %3, %51[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%53 = llvm.insertvalue %19, %52[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%54 = llvm.extractvalue %13[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%55 = llvm.extractvalue %13[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%56 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%57 = llvm.insertvalue %54, %56[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%58 = llvm.insertvalue %55, %57[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%59 = llvm.insertvalue %18, %58[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%60 = llvm.insertvalue %0, %59[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%61 = llvm.insertvalue %1, %60[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%62 = llvm.insertvalue %2, %61[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%63 = llvm.insertvalue %3, %62[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%64 = llvm.insertvalue %23, %63[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%65 = llvm.insertvalue %22, %64[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%66 = llvm.insertvalue %3, %65[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%67 = llvm.insertvalue %19, %66[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%68 = llvm.extractvalue %14[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%69 = llvm.extractvalue %14[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%70 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%71 = llvm.insertvalue %68, %70[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%72 = llvm.insertvalue %69, %71[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%73 = llvm.insertvalue %18, %72[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%74 = llvm.insertvalue %0, %73[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%75 = llvm.insertvalue %2, %74[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%76 = llvm.insertvalue %1, %75[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%77 = llvm.insertvalue %1, %76[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%78 = llvm.insertvalue %25, %77[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%79 = llvm.insertvalue %24, %78[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%80 = llvm.insertvalue %1, %79[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%81 = llvm.insertvalue %19, %80[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%82 = llvm.extractvalue %15[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%83 = llvm.extractvalue %15[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%84 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%85 = llvm.insertvalue %82, %84[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%86 = llvm.insertvalue %83, %85[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%87 = llvm.insertvalue %18, %86[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%88 = llvm.insertvalue %0, %87[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%89 = llvm.insertvalue %2, %88[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%90 = llvm.insertvalue %1, %89[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%91 = llvm.insertvalue %1, %90[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%92 = llvm.insertvalue %25, %91[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%93 = llvm.insertvalue %24, %92[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%94 = llvm.insertvalue %1, %93[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%95 = llvm.insertvalue %19, %94[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%96 = llvm.extractvalue %16[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%97 = llvm.extractvalue %16[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%98 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%99 = llvm.insertvalue %96, %98[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%100 = llvm.insertvalue %97, %99[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%101 = llvm.insertvalue %18, %100[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%102 = llvm.insertvalue %0, %101[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%103 = llvm.insertvalue %1, %102[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%104 = llvm.insertvalue %2, %103[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%105 = llvm.insertvalue %3, %104[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%106 = llvm.insertvalue %23, %105[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%107 = llvm.insertvalue %22, %106[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%108 = llvm.insertvalue %3, %107[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%109 = llvm.insertvalue %19, %108[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%110 = llvm.extractvalue %17[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%111 = llvm.extractvalue %17[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%112 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%113 = llvm.insertvalue %110, %112[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%114 = llvm.insertvalue %111, %113[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%115 = llvm.insertvalue %18, %114[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%116 = llvm.insertvalue %0, %115[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%117 = llvm.insertvalue %1, %116[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%118 = llvm.insertvalue %2, %117[3, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%119 = llvm.insertvalue %3, %118[3, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%120 = llvm.insertvalue %23, %119[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%121 = llvm.insertvalue %22, %120[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%122 = llvm.insertvalue %3, %121[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%123 = llvm.insertvalue %19, %122[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
%124 = llvm.call @bench_inv_sqrt_index(%3) : (i64) -> f32
"llvm.br"(%18)[^bb0] : (i64) -> ()
  ^bb0(%125: i64):
    %126 = "llvm.icmp"(%125, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%126)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%18)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.br"(%18)[^bb4] : (i64) -> ()
  ^bb3(%127: i64):
    %128 = "llvm.icmp"(%127, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%128)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb5:
    "llvm.br"(%18)[^bb7] : (i64) -> ()
  ^bb6:
    %129 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %130 = "llvm.add"(%125, %129) : (i64, i64) -> i64
    "llvm.br"(%130)[^bb0] : (i64) -> ()
  ^bb7(%131: i64):
    %132 = "llvm.icmp"(%131, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%132)[^bb8, ^bb9] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb8:
    "llvm.br"(%18)[^bb10] : (i64) -> ()
  ^bb9:
    %133 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %134 = "llvm.add"(%127, %133) : (i64, i64) -> i64
    "llvm.br"(%134)[^bb3] : (i64) -> ()
  ^bb10(%135: i64):
    %136 = "llvm.icmp"(%135, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%136)[^bb11, ^bb12] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb11:
    "llvm.br"(%18, %20)[^bb13] : (i64, f32) -> ()
  ^bb12:
    %137 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %138 = "llvm.add"(%131, %137) : (i64, i64) -> i64
    "llvm.br"(%138)[^bb7] : (i64) -> ()
  ^bb13(%139: i64, %140: f32):
    %141 = "llvm.icmp"(%139, %3) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%141)[^bb14, ^bb15] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb14:
    %142 = llvm.extractvalue %39[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %143 = llvm.extractvalue %39[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %144 = "llvm.mul"(%125, %143) : (i64, i64) -> i64
    %145 = llvm.extractvalue %39[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %146 = "llvm.mul"(%131, %145) : (i64, i64) -> i64
    %147 = llvm.extractvalue %39[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %148 = "llvm.mul"(%127, %147) : (i64, i64) -> i64
    %149 = llvm.extractvalue %39[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %150 = "llvm.mul"(%139, %149) : (i64, i64) -> i64
    %151 = "llvm.add"(%144, %146) : (i64, i64) -> i64
    %152 = "llvm.add"(%151, %148) : (i64, i64) -> i64
    %153 = "llvm.add"(%152, %150) : (i64, i64) -> i64
    %154 = "llvm.getelementptr"(%142, %153) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %155 = llvm.load %154 : !llvm.ptr -> f32
    %156 = llvm.extractvalue %53[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %157 = llvm.extractvalue %53[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %158 = "llvm.mul"(%125, %157) : (i64, i64) -> i64
    %159 = llvm.extractvalue %53[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %160 = "llvm.mul"(%135, %159) : (i64, i64) -> i64
    %161 = llvm.extractvalue %53[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %162 = "llvm.mul"(%127, %161) : (i64, i64) -> i64
    %163 = llvm.extractvalue %53[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %164 = "llvm.mul"(%139, %163) : (i64, i64) -> i64
    %165 = "llvm.add"(%158, %160) : (i64, i64) -> i64
    %166 = "llvm.add"(%165, %162) : (i64, i64) -> i64
    %167 = "llvm.add"(%166, %164) : (i64, i64) -> i64
    %168 = "llvm.getelementptr"(%156, %167) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %169 = llvm.load %168 : !llvm.ptr -> f32
    %170 = "llvm.fmul"(%155, %169) : (f32, f32) -> f32
    %171 = "llvm.fadd"(%140, %170) : (f32, f32) -> f32
    %172 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %173 = "llvm.add"(%139, %172) : (i64, i64) -> i64
    "llvm.br"(%173, %171)[^bb13] : (i64, f32) -> ()
  ^bb15:
    %174 = "llvm.fmul"(%140, %124) : (f32, f32) -> f32
    %175 = llvm.extractvalue %81[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %176 = llvm.extractvalue %81[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %177 = "llvm.mul"(%125, %176) : (i64, i64) -> i64
    %178 = llvm.extractvalue %81[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %179 = "llvm.mul"(%127, %178) : (i64, i64) -> i64
    %180 = llvm.extractvalue %81[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %181 = "llvm.mul"(%131, %180) : (i64, i64) -> i64
    %182 = llvm.extractvalue %81[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %183 = "llvm.mul"(%135, %182) : (i64, i64) -> i64
    %184 = "llvm.add"(%177, %179) : (i64, i64) -> i64
    %185 = "llvm.add"(%184, %181) : (i64, i64) -> i64
    %186 = "llvm.add"(%185, %183) : (i64, i64) -> i64
    %187 = "llvm.getelementptr"(%175, %186) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%174, %187) : (f32, !llvm.ptr) -> ()
    %188 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %189 = "llvm.add"(%135, %188) : (i64, i64) -> i64
    "llvm.br"(%189)[^bb10] : (i64) -> ()
  ^bb4(%190: i64):
    %191 = "llvm.icmp"(%190, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%191)[^bb16, ^bb17] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb16:
    "llvm.br"(%18)[^bb18] : (i64) -> ()
  ^bb17:
    "llvm.br"(%18)[^bb19] : (i64) -> ()
  ^bb18(%192: i64):
    %193 = "llvm.icmp"(%192, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%193)[^bb20, ^bb21] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb20:
    "llvm.br"(%18)[^bb22] : (i64) -> ()
  ^bb21:
    %194 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %195 = "llvm.add"(%190, %194) : (i64, i64) -> i64
    "llvm.br"(%195)[^bb4] : (i64) -> ()
  ^bb22(%196: i64):
    %197 = "llvm.icmp"(%196, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%197)[^bb23, ^bb24] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb23:
    "llvm.br"(%18, %21)[^bb25] : (i64, f32) -> ()
  ^bb24:
    %198 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %199 = "llvm.add"(%192, %198) : (i64, i64) -> i64
    "llvm.br"(%199)[^bb18] : (i64) -> ()
  ^bb25(%200: i64, %201: f32):
    %202 = "llvm.icmp"(%200, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%202)[^bb26, ^bb27] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb26:
    %203 = llvm.extractvalue %81[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %204 = llvm.extractvalue %81[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %205 = "llvm.mul"(%190, %204) : (i64, i64) -> i64
    %206 = llvm.extractvalue %81[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %207 = "llvm.mul"(%192, %206) : (i64, i64) -> i64
    %208 = llvm.extractvalue %81[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %209 = "llvm.mul"(%196, %208) : (i64, i64) -> i64
    %210 = llvm.extractvalue %81[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %211 = "llvm.mul"(%200, %210) : (i64, i64) -> i64
    %212 = "llvm.add"(%205, %207) : (i64, i64) -> i64
    %213 = "llvm.add"(%212, %209) : (i64, i64) -> i64
    %214 = "llvm.add"(%213, %211) : (i64, i64) -> i64
    %215 = "llvm.getelementptr"(%203, %214) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %216 = llvm.load %215 : !llvm.ptr -> f32
    %217 = "arith.maximumf"(%201, %216) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %218 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %219 = "llvm.add"(%200, %218) : (i64, i64) -> i64
    "llvm.br"(%219, %217)[^bb25] : (i64, f32) -> ()
  ^bb27:
    "llvm.br"(%18, %20)[^bb28] : (i64, f32) -> ()
  ^bb28(%220: i64, %221: f32):
    %222 = "llvm.icmp"(%220, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%222)[^bb29, ^bb30] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb29:
    %223 = llvm.extractvalue %81[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %224 = llvm.extractvalue %81[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %225 = "llvm.mul"(%190, %224) : (i64, i64) -> i64
    %226 = llvm.extractvalue %81[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %227 = "llvm.mul"(%192, %226) : (i64, i64) -> i64
    %228 = llvm.extractvalue %81[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %229 = "llvm.mul"(%196, %228) : (i64, i64) -> i64
    %230 = llvm.extractvalue %81[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %231 = "llvm.mul"(%220, %230) : (i64, i64) -> i64
    %232 = "llvm.add"(%225, %227) : (i64, i64) -> i64
    %233 = "llvm.add"(%232, %229) : (i64, i64) -> i64
    %234 = "llvm.add"(%233, %231) : (i64, i64) -> i64
    %235 = "llvm.getelementptr"(%223, %234) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %236 = llvm.load %235 : !llvm.ptr -> f32
    %237 = "arith.subf"(%236, %201) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %238 = llvm.call @bench_expf(%237) : (f32) -> f32
    %239 = llvm.extractvalue %95[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %240 = llvm.extractvalue %95[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %241 = "llvm.mul"(%190, %240) : (i64, i64) -> i64
    %242 = llvm.extractvalue %95[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %243 = "llvm.mul"(%192, %242) : (i64, i64) -> i64
    %244 = llvm.extractvalue %95[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %245 = "llvm.mul"(%196, %244) : (i64, i64) -> i64
    %246 = llvm.extractvalue %95[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %247 = "llvm.mul"(%220, %246) : (i64, i64) -> i64
    %248 = "llvm.add"(%241, %243) : (i64, i64) -> i64
    %249 = "llvm.add"(%248, %245) : (i64, i64) -> i64
    %250 = "llvm.add"(%249, %247) : (i64, i64) -> i64
    %251 = "llvm.getelementptr"(%239, %250) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%238, %251) : (f32, !llvm.ptr) -> ()
    %252 = "llvm.fadd"(%221, %238) : (f32, f32) -> f32
    %253 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %254 = "llvm.add"(%220, %253) : (i64, i64) -> i64
    "llvm.br"(%254, %252)[^bb28] : (i64, f32) -> ()
  ^bb30:
    "llvm.br"(%18)[^bb31] : (i64) -> ()
  ^bb31(%255: i64):
    %256 = "llvm.icmp"(%255, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%256)[^bb32, ^bb33] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb32:
    %257 = llvm.extractvalue %95[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %258 = llvm.extractvalue %95[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %259 = "llvm.mul"(%190, %258) : (i64, i64) -> i64
    %260 = llvm.extractvalue %95[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %261 = "llvm.mul"(%192, %260) : (i64, i64) -> i64
    %262 = llvm.extractvalue %95[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %263 = "llvm.mul"(%196, %262) : (i64, i64) -> i64
    %264 = llvm.extractvalue %95[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %265 = "llvm.mul"(%255, %264) : (i64, i64) -> i64
    %266 = "llvm.add"(%259, %261) : (i64, i64) -> i64
    %267 = "llvm.add"(%266, %263) : (i64, i64) -> i64
    %268 = "llvm.add"(%267, %265) : (i64, i64) -> i64
    %269 = "llvm.getelementptr"(%257, %268) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %270 = llvm.load %269 : !llvm.ptr -> f32
    %271 = "arith.divf"(%270, %221) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
    %272 = llvm.extractvalue %95[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %273 = llvm.extractvalue %95[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %274 = "llvm.mul"(%190, %273) : (i64, i64) -> i64
    %275 = llvm.extractvalue %95[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %276 = "llvm.mul"(%192, %275) : (i64, i64) -> i64
    %277 = llvm.extractvalue %95[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %278 = "llvm.mul"(%196, %277) : (i64, i64) -> i64
    %279 = llvm.extractvalue %95[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %280 = "llvm.mul"(%255, %279) : (i64, i64) -> i64
    %281 = "llvm.add"(%274, %276) : (i64, i64) -> i64
    %282 = "llvm.add"(%281, %278) : (i64, i64) -> i64
    %283 = "llvm.add"(%282, %280) : (i64, i64) -> i64
    %284 = "llvm.getelementptr"(%272, %283) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%271, %284) : (f32, !llvm.ptr) -> ()
    %285 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %286 = "llvm.add"(%255, %285) : (i64, i64) -> i64
    "llvm.br"(%286)[^bb31] : (i64) -> ()
  ^bb33:
    %287 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %288 = "llvm.add"(%196, %287) : (i64, i64) -> i64
    "llvm.br"(%288)[^bb22] : (i64) -> ()
  ^bb19(%289: i64):
    %290 = "llvm.icmp"(%289, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%290)[^bb34, ^bb35] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb34:
    "llvm.br"(%18)[^bb36] : (i64) -> ()
  ^bb35:
    "llvm.return"() : () -> ()
  ^bb36(%291: i64):
    %292 = "llvm.icmp"(%291, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%292)[^bb37, ^bb38] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb37:
    "llvm.br"(%18)[^bb39] : (i64) -> ()
  ^bb38:
    %293 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %294 = "llvm.add"(%289, %293) : (i64, i64) -> i64
    "llvm.br"(%294)[^bb19] : (i64) -> ()
  ^bb39(%295: i64):
    %296 = "llvm.icmp"(%295, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%296)[^bb40, ^bb41] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb40:
    "llvm.br"(%18)[^bb42] : (i64) -> ()
  ^bb41:
    "llvm.br"(%18)[^bb43] : (i64) -> ()
  ^bb42(%297: i64):
    %298 = "llvm.icmp"(%297, %3) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%298)[^bb44, ^bb45] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb44:
    "llvm.br"(%18, %20)[^bb46] : (i64, f32) -> ()
  ^bb45:
    %299 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %300 = "llvm.add"(%295, %299) : (i64, i64) -> i64
    "llvm.br"(%300)[^bb39] : (i64) -> ()
  ^bb46(%301: i64, %302: f32):
    %303 = "llvm.icmp"(%301, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%303)[^bb47, ^bb48] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb47:
    %304 = llvm.extractvalue %95[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %305 = llvm.extractvalue %95[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %306 = "llvm.mul"(%289, %305) : (i64, i64) -> i64
    %307 = llvm.extractvalue %95[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %308 = "llvm.mul"(%295, %307) : (i64, i64) -> i64
    %309 = llvm.extractvalue %95[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %310 = "llvm.mul"(%291, %309) : (i64, i64) -> i64
    %311 = llvm.extractvalue %95[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %312 = "llvm.mul"(%301, %311) : (i64, i64) -> i64
    %313 = "llvm.add"(%306, %308) : (i64, i64) -> i64
    %314 = "llvm.add"(%313, %310) : (i64, i64) -> i64
    %315 = "llvm.add"(%314, %312) : (i64, i64) -> i64
    %316 = "llvm.getelementptr"(%304, %315) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %317 = llvm.load %316 : !llvm.ptr -> f32
    %318 = llvm.extractvalue %67[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %319 = llvm.extractvalue %67[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %320 = "llvm.mul"(%289, %319) : (i64, i64) -> i64
    %321 = llvm.extractvalue %67[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %322 = "llvm.mul"(%301, %321) : (i64, i64) -> i64
    %323 = llvm.extractvalue %67[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %324 = "llvm.mul"(%295, %323) : (i64, i64) -> i64
    %325 = llvm.extractvalue %67[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %326 = "llvm.mul"(%297, %325) : (i64, i64) -> i64
    %327 = "llvm.add"(%320, %322) : (i64, i64) -> i64
    %328 = "llvm.add"(%327, %324) : (i64, i64) -> i64
    %329 = "llvm.add"(%328, %326) : (i64, i64) -> i64
    %330 = "llvm.getelementptr"(%318, %329) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %331 = llvm.load %330 : !llvm.ptr -> f32
    %332 = "llvm.fmul"(%317, %331) : (f32, f32) -> f32
    %333 = "llvm.fadd"(%302, %332) : (f32, f32) -> f32
    %334 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %335 = "llvm.add"(%301, %334) : (i64, i64) -> i64
    "llvm.br"(%335, %333)[^bb46] : (i64, f32) -> ()
  ^bb48:
    %336 = llvm.extractvalue %109[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %337 = llvm.extractvalue %109[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %338 = "llvm.mul"(%289, %337) : (i64, i64) -> i64
    %339 = llvm.extractvalue %109[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %340 = "llvm.mul"(%291, %339) : (i64, i64) -> i64
    %341 = llvm.extractvalue %109[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %342 = "llvm.mul"(%295, %341) : (i64, i64) -> i64
    %343 = llvm.extractvalue %109[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %344 = "llvm.mul"(%297, %343) : (i64, i64) -> i64
    %345 = "llvm.add"(%338, %340) : (i64, i64) -> i64
    %346 = "llvm.add"(%345, %342) : (i64, i64) -> i64
    %347 = "llvm.add"(%346, %344) : (i64, i64) -> i64
    %348 = "llvm.getelementptr"(%336, %347) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%302, %348) : (f32, !llvm.ptr) -> ()
    %349 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %350 = "llvm.add"(%297, %349) : (i64, i64) -> i64
    "llvm.br"(%350)[^bb42] : (i64) -> ()
  ^bb43(%351: i64):
    %352 = "llvm.icmp"(%351, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%352)[^bb49, ^bb50] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb49:
    "llvm.br"(%18)[^bb51] : (i64) -> ()
  ^bb50:
    %353 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %354 = "llvm.add"(%291, %353) : (i64, i64) -> i64
    "llvm.br"(%354)[^bb36] : (i64) -> ()
  ^bb51(%355: i64):
    %356 = "llvm.icmp"(%355, %3) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%356)[^bb52, ^bb53] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb52:
    %357 = llvm.extractvalue %109[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %358 = llvm.extractvalue %109[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %359 = "llvm.mul"(%289, %358) : (i64, i64) -> i64
    %360 = llvm.extractvalue %109[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %361 = "llvm.mul"(%291, %360) : (i64, i64) -> i64
    %362 = llvm.extractvalue %109[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %363 = "llvm.mul"(%351, %362) : (i64, i64) -> i64
    %364 = llvm.extractvalue %109[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %365 = "llvm.mul"(%355, %364) : (i64, i64) -> i64
    %366 = "llvm.add"(%359, %361) : (i64, i64) -> i64
    %367 = "llvm.add"(%366, %363) : (i64, i64) -> i64
    %368 = "llvm.add"(%367, %365) : (i64, i64) -> i64
    %369 = "llvm.getelementptr"(%357, %368) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %370 = llvm.load %369 : !llvm.ptr -> f32
    %371 = llvm.extractvalue %123[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %372 = llvm.extractvalue %123[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %373 = "llvm.mul"(%289, %372) : (i64, i64) -> i64
    %374 = llvm.extractvalue %123[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %375 = "llvm.mul"(%291, %374) : (i64, i64) -> i64
    %376 = llvm.extractvalue %123[4, 2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %377 = "llvm.mul"(%351, %376) : (i64, i64) -> i64
    %378 = llvm.extractvalue %123[4, 3] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<4 x i64>, !llvm.array<4 x i64>)>
    %379 = "llvm.mul"(%355, %378) : (i64, i64) -> i64
    %380 = "llvm.add"(%373, %375) : (i64, i64) -> i64
    %381 = "llvm.add"(%380, %377) : (i64, i64) -> i64
    %382 = "llvm.add"(%381, %379) : (i64, i64) -> i64
    %383 = "llvm.getelementptr"(%371, %382) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%370, %383) : (f32, !llvm.ptr) -> ()
    %384 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %385 = "llvm.add"(%355, %384) : (i64, i64) -> i64
    "llvm.br"(%385)[^bb51] : (i64) -> ()
  ^bb53:
    %386 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %387 = "llvm.add"(%351, %386) : (i64, i64) -> i64
    "llvm.br"(%387)[^bb43] : (i64) -> ()
  }
}

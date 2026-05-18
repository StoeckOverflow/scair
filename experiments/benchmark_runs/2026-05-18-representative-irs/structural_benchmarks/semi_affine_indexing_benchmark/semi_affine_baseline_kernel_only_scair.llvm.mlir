builtin.module {
  llvm.func @semi_affine_fill_and_sum(%0: i64, %1: i64, %2: i64, %3: i64, %4: !llvm.ptr, %5: !llvm.ptr) {
%6 = llvm.load %4 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%7 = llvm.load %5 : !llvm.ptr -> !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%8 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%9 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%10 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
%11 = llvm.extractvalue %6[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%12 = llvm.extractvalue %6[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
%13 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%14 = llvm.insertvalue %11, %13[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%15 = llvm.insertvalue %12, %14[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%16 = llvm.insertvalue %8, %15[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%17 = llvm.insertvalue %0, %16[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%18 = llvm.insertvalue %1, %17[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%19 = llvm.insertvalue %2, %18[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
%20 = llvm.insertvalue %3, %19[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
"llvm.br"(%8)[^bb0] : (i64) -> ()
  ^bb0(%21: i64):
    %22 = llvm.icmp "slt" %21, %0 : i64
    "llvm.cond_br"(%22)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%8)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.br"(%8, %9)[^bb4] : (i64, f32) -> ()
  ^bb3(%23: i64):
    %24 = llvm.icmp "slt" %23, %1 : i64
    "llvm.cond_br"(%24)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb5:
    %25 = llvm.extractvalue %20[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %26 = llvm.extractvalue %20[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %27 = "llvm.mul"(%21, %26) : (i64, i64) -> i64
    %28 = llvm.extractvalue %20[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %29 = "llvm.mul"(%23, %28) : (i64, i64) -> i64
    %30 = "llvm.add"(%27, %29) : (i64, i64) -> i64
    %31 = "llvm.getelementptr"(%25, %30) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%10, %31) : (f32, !llvm.ptr) -> ()
    %32 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %33 = "llvm.add"(%23, %32) : (i64, i64) -> i64
    "llvm.br"(%33)[^bb3] : (i64) -> ()
  ^bb6:
    %34 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %35 = "llvm.add"(%21, %34) : (i64, i64) -> i64
    "llvm.br"(%35)[^bb0] : (i64) -> ()
  ^bb4(%36: i64, %37: f32):
    %38 = llvm.icmp "slt" %36, %0 : i64
    "llvm.cond_br"(%38)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    "llvm.br"(%8, %37)[^bb9] : (i64, f32) -> ()
  ^bb9(%39: i64, %40: f32):
    %41 = llvm.icmp "slt" %39, %1 : i64
    "llvm.cond_br"(%41)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    %42 = llvm.extractvalue %20[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %43 = llvm.extractvalue %20[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %44 = "llvm.mul"(%36, %43) : (i64, i64) -> i64
    %45 = llvm.extractvalue %20[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
    %46 = "llvm.mul"(%39, %45) : (i64, i64) -> i64
    %47 = "llvm.add"(%44, %46) : (i64, i64) -> i64
    %48 = "llvm.getelementptr"(%42, %47) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %49 = llvm.load %48 : !llvm.ptr -> f32
    %50 = "llvm.fadd"(%40, %49) : (f32, f32) -> f32
    %51 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %52 = "llvm.add"(%39, %51) : (i64, i64) -> i64
    "llvm.br"(%52, %50)[^bb9] : (i64, f32) -> ()
  ^bb11:
    %53 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %54 = "llvm.add"(%36, %53) : (i64, i64) -> i64
    "llvm.br"(%54, %40)[^bb4] : (i64, f32) -> ()
  ^bb8:
    %55 = llvm.extractvalue %7[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
    %56 = llvm.extractvalue %7[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
    %57 = "llvm.mul"(%8, %56) : (i64, i64) -> i64
    %58 = "llvm.getelementptr"(%55, %57) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%37, %58) : (f32, !llvm.ptr) -> ()
    "llvm.return"() : () -> ()
  }
}

builtin.module {
  llvm.func @conv2d_dynamic(%0: i64, %1: i64, %2: i64, %3: i64, %4: i64, %5: i64, %6: i64, %7: i64, %8: i64, %9: !llvm.ptr, %10: !llvm.ptr, %11: !llvm.ptr) {
%12 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%13 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%14 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%15 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
%16 = "llvm.mul"(%1, %15) : (i64, i64) -> i64
%17 = "llvm.mul"(%5, %6) : (i64, i64) -> i64
%18 = "llvm.mul"(%1, %17) : (i64, i64) -> i64
%19 = "llvm.mul"(%7, %8) : (i64, i64) -> i64
%20 = "llvm.mul"(%4, %19) : (i64, i64) -> i64
"llvm.br"(%13)[^bb0] : (i64) -> ()
  ^bb0(%21: i64):
    %22 = llvm.icmp "slt" %21, %0 : i64
    "llvm.cond_br"(%22)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%13)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%23: i64):
    %24 = llvm.icmp "slt" %23, %4 : i64
    "llvm.cond_br"(%24)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    "llvm.br"(%13)[^bb6] : (i64) -> ()
  ^bb5:
    %25 = "llvm.add"(%21, %12) : (i64, i64) -> i64
    "llvm.br"(%25)[^bb0] : (i64) -> ()
  ^bb6(%26: i64):
    %27 = llvm.icmp "slt" %26, %7 : i64
    "llvm.cond_br"(%27)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    "llvm.br"(%13)[^bb9] : (i64) -> ()
  ^bb8:
    %28 = "llvm.add"(%23, %12) : (i64, i64) -> i64
    "llvm.br"(%28)[^bb3] : (i64) -> ()
  ^bb9(%29: i64):
    %30 = llvm.icmp "slt" %29, %8 : i64
    "llvm.cond_br"(%30)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    "llvm.br"(%13, %14)[^bb12] : (i64, f32) -> ()
  ^bb11:
    %31 = "llvm.add"(%26, %12) : (i64, i64) -> i64
    "llvm.br"(%31)[^bb6] : (i64) -> ()
  ^bb12(%32: i64, %33: f32):
    %34 = llvm.icmp "slt" %32, %1 : i64
    "llvm.cond_br"(%34)[^bb13, ^bb14] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb13:
    "llvm.br"(%32, %13, %33)[^bb15] : (i64, i64, f32) -> ()
  ^bb15(%35: i64, %36: i64, %37: f32):
    %38 = llvm.icmp "slt" %36, %5 : i64
    "llvm.cond_br"(%38)[^bb16, ^bb17] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb16:
    "llvm.br"(%13, %37)[^bb18] : (i64, f32) -> ()
  ^bb17:
    %39 = "llvm.add"(%35, %12) : (i64, i64) -> i64
    "llvm.br"(%39, %37)[^bb12] : (i64, f32) -> ()
  ^bb14:
    %40 = "llvm.mul"(%21, %20) : (i64, i64) -> i64
    %41 = "llvm.mul"(%23, %19) : (i64, i64) -> i64
    %42 = "llvm.mul"(%26, %8) : (i64, i64) -> i64
    %43 = "llvm.add"(%40, %41) : (i64, i64) -> i64
    %44 = "llvm.add"(%43, %42) : (i64, i64) -> i64
    %45 = "llvm.add"(%44, %29) : (i64, i64) -> i64
    %46 = "llvm.getelementptr"(%11, %45) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%33, %46) : (f32, !llvm.ptr) -> ()
    %47 = "llvm.add"(%29, %12) : (i64, i64) -> i64
    "llvm.br"(%47)[^bb9] : (i64) -> ()
  ^bb18(%48: i64, %49: f32):
    %50 = llvm.icmp "slt" %48, %6 : i64
    "llvm.cond_br"(%50)[^bb19, ^bb20] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb19:
    %51 = "llvm.mul"(%21, %16) : (i64, i64) -> i64
    %52 = "llvm.mul"(%35, %15) : (i64, i64) -> i64
    %53 = "llvm.mul"(%26, %3) : (i64, i64) -> i64
    %54 = "llvm.mul"(%36, %3) : (i64, i64) -> i64
    %55 = "llvm.add"(%51, %52) : (i64, i64) -> i64
    %56 = "llvm.add"(%55, %53) : (i64, i64) -> i64
    %57 = "llvm.add"(%56, %29) : (i64, i64) -> i64
    %58 = "llvm.add"(%57, %54) : (i64, i64) -> i64
    %59 = "llvm.add"(%58, %48) : (i64, i64) -> i64
    %60 = "llvm.getelementptr"(%9, %59) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %61 = llvm.load %60 : !llvm.ptr -> f32
    %62 = "llvm.mul"(%23, %18) : (i64, i64) -> i64
    %63 = "llvm.mul"(%35, %17) : (i64, i64) -> i64
    %64 = "llvm.mul"(%36, %6) : (i64, i64) -> i64
    %65 = "llvm.add"(%62, %63) : (i64, i64) -> i64
    %66 = "llvm.add"(%65, %64) : (i64, i64) -> i64
    %67 = "llvm.add"(%66, %48) : (i64, i64) -> i64
    %68 = "llvm.getelementptr"(%10, %67) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %69 = llvm.load %68 : !llvm.ptr -> f32
    %70 = "llvm.fmul"(%61, %69) : (f32, f32) -> f32
    %71 = "llvm.fadd"(%49, %70) : (f32, f32) -> f32
    %72 = "llvm.add"(%48, %12) : (i64, i64) -> i64
    "llvm.br"(%72, %71)[^bb18] : (i64, f32) -> ()
  ^bb20:
    %73 = "llvm.add"(%36, %12) : (i64, i64) -> i64
    "llvm.br"(%35, %73, %49)[^bb15] : (i64, i64, f32) -> ()
  }
}

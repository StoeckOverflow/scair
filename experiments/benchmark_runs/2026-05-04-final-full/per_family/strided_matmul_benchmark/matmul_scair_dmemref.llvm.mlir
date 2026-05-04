builtin.module {
  llvm.func @matmul_strided(%0: i64, %1: i64, %2: i64, %3: i64, %4: i64, %5: i64, %6: i64, %7: i64, %8: i64, %9: !llvm.ptr, %10: !llvm.ptr, %11: !llvm.ptr) {
%12 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%13 = "llvm.add"(%3, %12) : (i64, i64) -> i64
%14 = "llvm.add"(%4, %12) : (i64, i64) -> i64
%15 = "llvm.add"(%5, %12) : (i64, i64) -> i64
%16 = "llvm.add"(%6, %12) : (i64, i64) -> i64
%17 = "llvm.add"(%7, %12) : (i64, i64) -> i64
%18 = "llvm.add"(%8, %12) : (i64, i64) -> i64
%19 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%20 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%21 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
"llvm.br"(%20)[^bb0] : (i64) -> ()
  ^bb0(%22: i64):
    %23 = "llvm.icmp"(%22, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%23)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%20)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%24: i64):
    %25 = "llvm.icmp"(%24, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%25)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    "llvm.br"(%20, %21)[^bb6] : (i64, f32) -> ()
  ^bb5:
    %26 = "llvm.add"(%22, %19) : (i64, i64) -> i64
    "llvm.br"(%26)[^bb0] : (i64) -> ()
  ^bb6(%27: i64, %28: f32):
    %29 = "llvm.icmp"(%27, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%29)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    %30 = "llvm.mul"(%22, %13) : (i64, i64) -> i64
    %31 = "llvm.mul"(%27, %14) : (i64, i64) -> i64
    %32 = "llvm.add"(%30, %31) : (i64, i64) -> i64
    %33 = "llvm.getelementptr"(%9, %32) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %34 = llvm.load %33 : !llvm.ptr -> f32
    %35 = "llvm.mul"(%27, %15) : (i64, i64) -> i64
    %36 = "llvm.mul"(%24, %16) : (i64, i64) -> i64
    %37 = "llvm.add"(%35, %36) : (i64, i64) -> i64
    %38 = "llvm.getelementptr"(%10, %37) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %39 = llvm.load %38 : !llvm.ptr -> f32
    %40 = "llvm.fmul"(%34, %39) : (f32, f32) -> f32
    %41 = "llvm.fadd"(%28, %40) : (f32, f32) -> f32
    %42 = "llvm.add"(%27, %19) : (i64, i64) -> i64
    "llvm.br"(%42, %41)[^bb6] : (i64, f32) -> ()
  ^bb8:
    %43 = "llvm.mul"(%22, %17) : (i64, i64) -> i64
    %44 = "llvm.mul"(%24, %18) : (i64, i64) -> i64
    %45 = "llvm.add"(%43, %44) : (i64, i64) -> i64
    %46 = "llvm.getelementptr"(%11, %45) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%28, %46) : (f32, !llvm.ptr) -> ()
    %47 = "llvm.add"(%24, %19) : (i64, i64) -> i64
    "llvm.br"(%47)[^bb3] : (i64) -> ()
  }
}

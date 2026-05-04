builtin.module {
  llvm.func @matmul_tiling(%0: i64, %1: i64, %2: i64, %3: i64, %4: !llvm.ptr, %5: !llvm.ptr, %6: !llvm.ptr) {
%7 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%8 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%9 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%10 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
"llvm.br"(%8)[^bb0] : (i64) -> ()
  ^bb0(%11: i64):
    %12 = "llvm.icmp"(%11, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%12)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%8)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%13: i64):
    %14 = "llvm.icmp"(%13, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%14)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    %15 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
    "llvm.br"(%15, %9)[^bb6] : (i64, f32) -> ()
  ^bb5:
    %16 = "llvm.add"(%11, %7) : (i64, i64) -> i64
    "llvm.br"(%16)[^bb0] : (i64) -> ()
  ^bb6(%17: i64, %18: f32):
    %19 = "llvm.icmp"(%17, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%19)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    "llvm.br"(%17, %15, %18)[^bb9] : (i64, i64, f32) -> ()
  ^bb9(%20: i64, %21: i64, %22: f32):
    %23 = "llvm.icmp"(%21, %3) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%23)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    %24 = "llvm.mul"(%20, %3) : (i64, i64) -> i64
    %25 = "llvm.add"(%24, %21) : (i64, i64) -> i64
    %26 = "llvm.mul"(%11, %10) : (i64, i64) -> i64
    %27 = "llvm.add"(%26, %25) : (i64, i64) -> i64
    %28 = "llvm.getelementptr"(%4, %27) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %29 = llvm.load %28 : !llvm.ptr -> f32
    %30 = "llvm.mul"(%25, %1) : (i64, i64) -> i64
    %31 = "llvm.add"(%30, %13) : (i64, i64) -> i64
    %32 = "llvm.getelementptr"(%5, %31) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %33 = llvm.load %32 : !llvm.ptr -> f32
    %34 = "llvm.fmul"(%29, %33) : (f32, f32) -> f32
    %35 = "llvm.fadd"(%22, %34) : (f32, f32) -> f32
    %36 = "llvm.add"(%21, %7) : (i64, i64) -> i64
    "llvm.br"(%20, %36, %35)[^bb9] : (i64, i64, f32) -> ()
  ^bb11:
    %37 = "llvm.add"(%20, %7) : (i64, i64) -> i64
    "llvm.br"(%37, %22)[^bb6] : (i64, f32) -> ()
  ^bb8:
    %38 = "llvm.mul"(%11, %1) : (i64, i64) -> i64
    %39 = "llvm.add"(%38, %13) : (i64, i64) -> i64
    %40 = "llvm.getelementptr"(%6, %39) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%18, %40) : (f32, !llvm.ptr) -> ()
    %41 = "llvm.add"(%13, %7) : (i64, i64) -> i64
    "llvm.br"(%41)[^bb3] : (i64) -> ()
  }
}

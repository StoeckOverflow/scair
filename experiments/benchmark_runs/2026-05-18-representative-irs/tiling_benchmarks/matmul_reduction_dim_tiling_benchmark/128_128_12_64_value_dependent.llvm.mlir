builtin.module {
  llvm.func @matmul_reduction_dim_tiling(%0: i64, %1: i64, %2: i64, %3: i64, %4: !llvm.ptr, %5: !llvm.ptr, %6: !llvm.ptr) {
%7 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%8 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%9 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%10 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
"llvm.br"(%8)[^bb0] : (i64) -> ()
  ^bb0(%11: i64):
    %12 = llvm.icmp "slt" %11, %0 : i64
    "llvm.cond_br"(%12)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%8)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%13: i64):
    %14 = llvm.icmp "slt" %13, %1 : i64
    "llvm.cond_br"(%14)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    %15 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
    "llvm.br"(%15, %9)[^bb6] : (i64, f32) -> ()
  ^bb5:
    %16 = "llvm.add"(%11, %7) : (i64, i64) -> i64
    "llvm.br"(%16)[^bb0] : (i64) -> ()
  ^bb6(%17: i64, %18: f32):
    %19 = llvm.icmp "slt" %17, %2 : i64
    "llvm.cond_br"(%19)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    %20 = "llvm.mul"(%17, %3) : (i64, i64) -> i64
    %21 = "llvm.add"(%20, %3) : (i64, i64) -> i64
    "llvm.br"(%17, %20, %18, %20, %21)[^bb9] : (i64, i64, f32, i64, i64) -> ()
  ^bb9(%22: i64, %23: i64, %24: f32, %25: i64, %26: i64):
    %27 = llvm.icmp "slt" %23, %26 : i64
    "llvm.cond_br"(%27)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    %28 = "llvm.mul"(%11, %10) : (i64, i64) -> i64
    %29 = "llvm.add"(%28, %23) : (i64, i64) -> i64
    %30 = "llvm.getelementptr"(%4, %29) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %31 = llvm.load %30 : !llvm.ptr -> f32
    %32 = "llvm.mul"(%23, %1) : (i64, i64) -> i64
    %33 = "llvm.add"(%32, %13) : (i64, i64) -> i64
    %34 = "llvm.getelementptr"(%5, %33) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %35 = llvm.load %34 : !llvm.ptr -> f32
    %36 = "llvm.fmul"(%31, %35) : (f32, f32) -> f32
    %37 = "llvm.fadd"(%24, %36) : (f32, f32) -> f32
    %38 = "llvm.add"(%23, %7) : (i64, i64) -> i64
    "llvm.br"(%22, %38, %37, %25, %26)[^bb9] : (i64, i64, f32, i64, i64) -> ()
  ^bb11:
    %39 = "llvm.add"(%22, %7) : (i64, i64) -> i64
    "llvm.br"(%39, %24)[^bb6] : (i64, f32) -> ()
  ^bb8:
    %40 = "llvm.mul"(%11, %1) : (i64, i64) -> i64
    %41 = "llvm.add"(%40, %13) : (i64, i64) -> i64
    %42 = "llvm.getelementptr"(%6, %41) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%18, %42) : (f32, !llvm.ptr) -> ()
    %43 = "llvm.add"(%13, %7) : (i64, i64) -> i64
    "llvm.br"(%43)[^bb3] : (i64) -> ()
  }
}

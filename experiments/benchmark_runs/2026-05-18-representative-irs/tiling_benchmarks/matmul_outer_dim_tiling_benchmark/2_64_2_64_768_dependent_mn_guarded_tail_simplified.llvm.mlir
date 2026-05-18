builtin.module {
  llvm.func @matmul_outer_dim_tiling(%0: i64, %1: i64, %2: i64, %3: i64, %4: i64, %5: !llvm.ptr, %6: !llvm.ptr, %7: !llvm.ptr) {
%8 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%9 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%10 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%11 = "llvm.mul"(%0, %1) : (i64, i64) -> i64
%12 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
"llvm.br"(%9)[^bb0] : (i64) -> ()
  ^bb0(%13: i64):
    %14 = llvm.icmp "slt" %13, %11 : i64
    "llvm.cond_br"(%14)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    %15 = "llvm.add"(%13, %1) : (i64, i64) -> i64
    "llvm.br"(%13)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%16: i64):
    %17 = llvm.icmp "slt" %16, %15 : i64
    "llvm.cond_br"(%17)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    "llvm.br"(%9)[^bb6] : (i64) -> ()
  ^bb5:
    %18 = "llvm.add"(%13, %1) : (i64, i64) -> i64
    "llvm.br"(%18)[^bb0] : (i64) -> ()
  ^bb6(%19: i64):
    %20 = llvm.icmp "slt" %19, %12 : i64
    "llvm.cond_br"(%20)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    %21 = "llvm.add"(%19, %3) : (i64, i64) -> i64
    "llvm.br"(%19)[^bb9] : (i64) -> ()
  ^bb8:
    %22 = "llvm.add"(%16, %8) : (i64, i64) -> i64
    "llvm.br"(%22)[^bb3] : (i64) -> ()
  ^bb9(%23: i64):
    %24 = llvm.icmp "slt" %23, %21 : i64
    "llvm.cond_br"(%24)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    "llvm.br"(%9, %10)[^bb12] : (i64, f32) -> ()
  ^bb11:
    %25 = "llvm.add"(%19, %3) : (i64, i64) -> i64
    "llvm.br"(%25)[^bb6] : (i64) -> ()
  ^bb12(%26: i64, %27: f32):
    %28 = llvm.icmp "slt" %26, %4 : i64
    "llvm.cond_br"(%28)[^bb13, ^bb14] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb13:
    %29 = "llvm.mul"(%16, %4) : (i64, i64) -> i64
    %30 = "llvm.add"(%29, %26) : (i64, i64) -> i64
    %31 = "llvm.getelementptr"(%5, %30) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %32 = llvm.load %31 : !llvm.ptr -> f32
    %33 = "llvm.mul"(%26, %12) : (i64, i64) -> i64
    %34 = "llvm.add"(%33, %23) : (i64, i64) -> i64
    %35 = "llvm.getelementptr"(%6, %34) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %36 = llvm.load %35 : !llvm.ptr -> f32
    %37 = "llvm.fmul"(%32, %36) : (f32, f32) -> f32
    %38 = "llvm.fadd"(%27, %37) : (f32, f32) -> f32
    %39 = "llvm.add"(%26, %8) : (i64, i64) -> i64
    "llvm.br"(%39, %38)[^bb12] : (i64, f32) -> ()
  ^bb14:
    %40 = "llvm.mul"(%16, %12) : (i64, i64) -> i64
    %41 = "llvm.add"(%40, %23) : (i64, i64) -> i64
    %42 = "llvm.getelementptr"(%7, %41) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%27, %42) : (f32, !llvm.ptr) -> ()
    %43 = "llvm.add"(%23, %8) : (i64, i64) -> i64
    "llvm.br"(%43)[^bb9] : (i64) -> ()
  }
}

builtin.module {
  llvm.func @semi_affine_fill_and_sum(%0: i64, %1: i64, %2: !llvm.ptr, %3: !llvm.ptr) {
%4 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%5 = "llvm.add"(%0, %4) : (i64, i64) -> i64
%6 = "llvm.add"(%1, %4) : (i64, i64) -> i64
%7 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%8 = "llvm.mlir.constant"() <{value = 256}> : () -> i64
%9 = "llvm.mlir.constant"() <{value = 1024}> : () -> i64
%10 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%11 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%12 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
"llvm.br"(%10)[^bb0] : (i64) -> ()
  ^bb0(%13: i64):
    %14 = "llvm.icmp"(%13, %8) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%14)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    "llvm.br"(%10)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.br"(%10, %11)[^bb4] : (i64, f32) -> ()
  ^bb3(%15: i64):
    %16 = "llvm.icmp"(%15, %9) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%16)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb5:
    %17 = "llvm.mul"(%13, %5) : (i64, i64) -> i64
    %18 = "llvm.mul"(%15, %6) : (i64, i64) -> i64
    %19 = "llvm.add"(%17, %18) : (i64, i64) -> i64
    %20 = "llvm.getelementptr"(%2, %19) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%12, %20) : (f32, !llvm.ptr) -> ()
    %21 = "llvm.add"(%15, %7) : (i64, i64) -> i64
    "llvm.br"(%21)[^bb3] : (i64) -> ()
  ^bb6:
    %22 = "llvm.add"(%13, %7) : (i64, i64) -> i64
    "llvm.br"(%22)[^bb0] : (i64) -> ()
  ^bb4(%23: i64, %24: f32):
    %25 = "llvm.icmp"(%23, %8) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%25)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    "llvm.br"(%23, %10, %24)[^bb9] : (i64, i64, f32) -> ()
  ^bb9(%26: i64, %27: i64, %28: f32):
    %29 = "llvm.icmp"(%27, %9) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%29)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    %30 = "llvm.mul"(%26, %5) : (i64, i64) -> i64
    %31 = "llvm.mul"(%27, %6) : (i64, i64) -> i64
    %32 = "llvm.add"(%30, %31) : (i64, i64) -> i64
    %33 = "llvm.getelementptr"(%2, %32) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %34 = llvm.load %33 : !llvm.ptr -> f32
    %35 = "llvm.fadd"(%28, %34) : (f32, f32) -> f32
    %36 = "llvm.add"(%27, %7) : (i64, i64) -> i64
    "llvm.br"(%26, %36, %35)[^bb9] : (i64, i64, f32) -> ()
  ^bb11:
    %37 = "llvm.add"(%26, %7) : (i64, i64) -> i64
    "llvm.br"(%37, %28)[^bb4] : (i64, f32) -> ()
  ^bb8:
    %38 = "llvm.getelementptr"(%3, %10) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%24, %38) : (f32, !llvm.ptr) -> ()
    "llvm.return"() : () -> ()
  }
}

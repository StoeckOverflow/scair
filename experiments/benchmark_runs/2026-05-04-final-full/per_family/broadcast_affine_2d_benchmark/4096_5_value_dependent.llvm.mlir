builtin.module {
  llvm.func @broadcast_affine_2d(%0: i64, %1: i64, %2: !llvm.ptr, %3: !llvm.ptr, %4: !llvm.ptr, %5: !llvm.ptr) {
%6 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%7 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%8 = "llvm.mul"(%0, %1) : (i64, i64) -> i64
"llvm.br"(%7)[^bb0] : (i64) -> ()
  ^bb0(%9: i64):
    %10 = "llvm.icmp"(%9, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%10)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    %11 = "llvm.mul"(%9, %1) : (i64, i64) -> i64
    "llvm.br"(%7)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%12: i64):
    %13 = "llvm.icmp"(%12, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%13)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    %14 = "llvm.add"(%11, %12) : (i64, i64) -> i64
    %15 = "llvm.getelementptr"(%2, %14) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    %16 = llvm.load %15 : !llvm.ptr -> i64
    %17 = "llvm.getelementptr"(%3, %12) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    %18 = llvm.load %17 : !llvm.ptr -> i64
    %19 = "llvm.getelementptr"(%4, %12) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    %20 = llvm.load %19 : !llvm.ptr -> i64
    %21 = "llvm.mul"(%16, %18) : (i64, i64) -> i64
    %22 = "llvm.add"(%21, %20) : (i64, i64) -> i64
    %23 = "llvm.getelementptr"(%5, %14) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%22, %23) : (i64, !llvm.ptr) -> ()
    %24 = "llvm.add"(%12, %6) : (i64, i64) -> i64
    "llvm.br"(%24)[^bb3] : (i64) -> ()
  ^bb5:
    %25 = "llvm.add"(%9, %6) : (i64, i64) -> i64
    "llvm.br"(%25)[^bb0] : (i64) -> ()
  }
}

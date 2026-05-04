builtin.module {
  llvm.func @blocked_pack(%0: i64, %1: i64, %2: i64, %3: i64, %4: !llvm.ptr, %5: !llvm.ptr) {
%6 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%7 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%8 = "llvm.mul"(%0, %2) : (i64, i64) -> i64
%9 = "llvm.mul"(%1, %3) : (i64, i64) -> i64
%10 = "llvm.mul"(%3, %6) : (i64, i64) -> i64
%11 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
%12 = "llvm.mul"(%1, %11) : (i64, i64) -> i64
"llvm.br"(%7)[^bb0] : (i64) -> ()
  ^bb0(%13: i64):
    %14 = "llvm.icmp"(%13, %0) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%14)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    %15 = "llvm.mul"(%13, %2) : (i64, i64) -> i64
    "llvm.br"(%7)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%16: i64):
    %17 = "llvm.icmp"(%16, %1) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%17)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    %18 = "llvm.mul"(%16, %3) : (i64, i64) -> i64
    "llvm.br"(%7)[^bb6] : (i64) -> ()
  ^bb5:
    %19 = "llvm.add"(%13, %6) : (i64, i64) -> i64
    "llvm.br"(%19)[^bb0] : (i64) -> ()
  ^bb6(%20: i64):
    %21 = "llvm.icmp"(%20, %2) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%21)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    %22 = "llvm.add"(%15, %20) : (i64, i64) -> i64
    "llvm.br"(%7)[^bb9] : (i64) -> ()
  ^bb8:
    %23 = "llvm.add"(%16, %6) : (i64, i64) -> i64
    "llvm.br"(%23)[^bb3] : (i64) -> ()
  ^bb9(%24: i64):
    %25 = "llvm.icmp"(%24, %3) <{predicate = 2}> : (i64, i64) -> i1
    "llvm.cond_br"(%25)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    %26 = "llvm.add"(%18, %24) : (i64, i64) -> i64
    %27 = "llvm.mul"(%22, %9) : (i64, i64) -> i64
    %28 = "llvm.add"(%27, %26) : (i64, i64) -> i64
    %29 = "llvm.getelementptr"(%4, %28) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    %30 = llvm.load %29 : !llvm.ptr -> i64
    %31 = "llvm.mul"(%13, %12) : (i64, i64) -> i64
    %32 = "llvm.mul"(%16, %11) : (i64, i64) -> i64
    %33 = "llvm.mul"(%20, %10) : (i64, i64) -> i64
    %34 = "llvm.add"(%31, %32) : (i64, i64) -> i64
    %35 = "llvm.add"(%34, %33) : (i64, i64) -> i64
    %36 = "llvm.add"(%35, %24) : (i64, i64) -> i64
    %37 = "llvm.getelementptr"(%5, %36) <{rawConstantIndices = array<i32: -2147483648>, elem_type = i64}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%30, %37) : (i64, !llvm.ptr) -> ()
    %38 = "llvm.add"(%24, %6) : (i64, i64) -> i64
    "llvm.br"(%38)[^bb9] : (i64) -> ()
  ^bb11:
    %39 = "llvm.add"(%20, %6) : (i64, i64) -> i64
    "llvm.br"(%39)[^bb6] : (i64) -> ()
  }
}

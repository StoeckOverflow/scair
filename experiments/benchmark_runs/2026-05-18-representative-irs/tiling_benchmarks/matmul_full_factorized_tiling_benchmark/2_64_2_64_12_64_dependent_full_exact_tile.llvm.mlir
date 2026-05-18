builtin.module {
  llvm.func @matmul_full_factorized_tiling(%0: i64, %1: i64, %2: i64, %3: i64, %4: i64, %5: i64, %6: !llvm.ptr, %7: !llvm.ptr, %8: !llvm.ptr) {
%9 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
%10 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
%11 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
%12 = "llvm.mul"(%0, %1) : (i64, i64) -> i64
%13 = "llvm.mul"(%2, %3) : (i64, i64) -> i64
%14 = "llvm.mul"(%4, %5) : (i64, i64) -> i64
"llvm.br"(%10)[^bb0] : (i64) -> ()
  ^bb0(%15: i64):
    %16 = llvm.icmp "slt" %15, %12 : i64
    "llvm.cond_br"(%16)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    %17 = "llvm.add"(%15, %1) : (i64, i64) -> i64
    "llvm.br"(%15)[^bb3] : (i64) -> ()
  ^bb2:
    "llvm.return"() : () -> ()
  ^bb3(%18: i64):
    %19 = llvm.icmp "slt" %18, %17 : i64
    "llvm.cond_br"(%19)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb4:
    "llvm.br"(%10)[^bb6] : (i64) -> ()
  ^bb5:
    %20 = "llvm.add"(%15, %1) : (i64, i64) -> i64
    "llvm.br"(%20)[^bb0] : (i64) -> ()
  ^bb6(%21: i64):
    %22 = llvm.icmp "slt" %21, %13 : i64
    "llvm.cond_br"(%22)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb7:
    %23 = "llvm.add"(%21, %3) : (i64, i64) -> i64
    "llvm.br"(%21)[^bb9] : (i64) -> ()
  ^bb8:
    %24 = "llvm.add"(%18, %9) : (i64, i64) -> i64
    "llvm.br"(%24)[^bb3] : (i64) -> ()
  ^bb9(%25: i64):
    %26 = llvm.icmp "slt" %25, %23 : i64
    "llvm.cond_br"(%26)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb10:
    "llvm.br"(%10, %11)[^bb12] : (i64, f32) -> ()
  ^bb11:
    %27 = "llvm.add"(%21, %3) : (i64, i64) -> i64
    "llvm.br"(%27)[^bb6] : (i64) -> ()
  ^bb12(%28: i64, %29: f32):
    %30 = llvm.icmp "slt" %28, %14 : i64
    "llvm.cond_br"(%30)[^bb13, ^bb14] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb13:
    %31 = "llvm.add"(%28, %5) : (i64, i64) -> i64
    "llvm.br"(%28, %28, %29, %31)[^bb15] : (i64, i64, f32, i64) -> ()
  ^bb15(%32: i64, %33: i64, %34: f32, %35: i64):
    %36 = llvm.icmp "slt" %33, %35 : i64
    "llvm.cond_br"(%36)[^bb16, ^bb17] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb16:
    %37 = "llvm.mul"(%18, %14) : (i64, i64) -> i64
    %38 = "llvm.add"(%37, %33) : (i64, i64) -> i64
    %39 = "llvm.getelementptr"(%6, %38) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %40 = llvm.load %39 : !llvm.ptr -> f32
    %41 = "llvm.mul"(%33, %13) : (i64, i64) -> i64
    %42 = "llvm.add"(%41, %25) : (i64, i64) -> i64
    %43 = "llvm.getelementptr"(%7, %42) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %44 = llvm.load %43 : !llvm.ptr -> f32
    %45 = "llvm.fmul"(%40, %44) : (f32, f32) -> f32
    %46 = "llvm.fadd"(%34, %45) : (f32, f32) -> f32
    %47 = "llvm.add"(%33, %9) : (i64, i64) -> i64
    "llvm.br"(%32, %47, %46, %35)[^bb15] : (i64, i64, f32, i64) -> ()
  ^bb17:
    %48 = "llvm.add"(%32, %5) : (i64, i64) -> i64
    "llvm.br"(%48, %34)[^bb12] : (i64, f32) -> ()
  ^bb14:
    %49 = "llvm.mul"(%18, %13) : (i64, i64) -> i64
    %50 = "llvm.add"(%49, %25) : (i64, i64) -> i64
    %51 = "llvm.getelementptr"(%8, %50) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%29, %51) : (f32, !llvm.ptr) -> ()
    %52 = "llvm.add"(%25, %9) : (i64, i64) -> i64
    "llvm.br"(%52)[^bb9] : (i64) -> ()
  }
}

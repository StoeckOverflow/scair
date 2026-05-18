builtin.module {
  func.func @runtime_checked_dynamic(%0: index, %1: index) -> index {
    %2 = "arith.constant"() <{value = 0 : index}> : () -> index
    %3 = "arith.cmpi"(%1, %2) <{predicate = 4}> : (index, index) -> i1
    "llvm.cond_br"(%3)[^bb0, ^bb1] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb1:
    llvm.call @abort() : () -> ()
    "llvm.unreachable"() : () -> ()
  ^bb0:
    %4 = "arith.muli"(%0, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %5 = "arith.constant"() <{value = 0 : index}> : () -> index
    "llvm.br"(%5, %2)[^bb2] : (index, index) -> ()
  ^bb2(%6: index, %7: index):
    %8 = llvm.icmp "slt" %6, %4 : index
    "llvm.cond_br"(%8)[^bb3, ^bb4] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb3:
    %9 = "arith.addi"(%6, %1) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    "llvm.br"(%6, %6, %7, %9)[^bb5] : (index, index, index, index) -> ()
  ^bb5(%10: index, %11: index, %12: index, %13: index):
    %14 = llvm.icmp "slt" %11, %13 : index
    "llvm.cond_br"(%14)[^bb6, ^bb7] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
  ^bb6:
    %15 = "llvm.add"(%11, %12) : (index, index) -> i64
    %16 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
    %17 = "llvm.add"(%11, %16) : (index, i64) -> i64
    "llvm.br"(%10, %17, %15, %13)[^bb5] : (index, i64, i64, index) -> ()
  ^bb7:
    %18 = "llvm.add"(%10, %1) : (index, index) -> i64
    "llvm.br"(%18, %12)[^bb2] : (i64, index) -> ()
  ^bb4:
    func.return %7 : index
  }
}

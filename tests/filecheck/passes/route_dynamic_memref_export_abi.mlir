// RUN: scair-opt %s -p convert-llvm-export-abi | filecheck %s

"builtin.module"() ({
  "llvm.func"() <{sym_name = "dynamic_copy", function_type = (i64, !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>) -> ()}> ({
  ^bb0(%0: i64, %1: !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>):
    %2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
    %3 = "llvm.extractvalue"(%1) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>) -> !llvm.ptr
    %4 = "llvm.extractvalue"(%1) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>) -> i64
    %5 = "llvm.mul"(%2, %4) : (i64, i64) -> i64
    %6 = "llvm.getelementptr"(%3, %5) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %7 = "llvm.load"(%6) : (!llvm.ptr) -> f32
    %8 = "llvm.extractvalue"(%1) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>) -> !llvm.ptr
    %9 = "llvm.extractvalue"(%1) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>) -> i64
    %10 = "llvm.mul"(%2, %9) : (i64, i64) -> i64
    %11 = "llvm.getelementptr"(%8, %10) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%7, %11) : (f32, !llvm.ptr) -> ()
    "llvm.return"() : () -> ()
  }) {scair.emit_bare_interface = true} : () -> ()
}) : () -> ()

// CHECK-LABEL: builtin.module {
// CHECK-NEXT:  llvm.func @dynamic_copy(%0: i64, %1: !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>) {
// CHECK-NEXT:%2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK-NEXT:%3 = llvm.extractvalue %1[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// CHECK-NEXT:%4 = llvm.extractvalue %1[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// CHECK-NEXT:%5 = "llvm.mul"(%2, %4) : (i64, i64) -> i64
// CHECK-NEXT:%6 = "llvm.getelementptr"(%3, %5) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK-NEXT:%7 = llvm.load %6 : !llvm.ptr -> f32
// CHECK-NEXT:%8 = llvm.extractvalue %1[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// CHECK-NEXT:%9 = llvm.extractvalue %1[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// CHECK-NEXT:%10 = "llvm.mul"(%2, %9) : (i64, i64) -> i64
// CHECK-NEXT:%11 = "llvm.getelementptr"(%8, %10) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK-NEXT:"llvm.store"(%7, %11) : (f32, !llvm.ptr) -> ()
// CHECK-NEXT:"llvm.return"() : () -> ()
// CHECK-NEXT:  }
// CHECK-NEXT:}

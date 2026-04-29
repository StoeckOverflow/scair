// RUN: scair-opt %s -p convert-llvm-export-abi | filecheck %s

builtin.module {
  llvm.func @refined_copy(%0: i64, %1: !llvm.ptr) attributes {scair.emit_bare_interface = true} {
    %2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
    %3 = "llvm.getelementptr"(%1, %2) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    %4 = llvm.load %3 : !llvm.ptr -> f32
    %5 = "llvm.getelementptr"(%1, %2) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
    "llvm.store"(%4, %5) : (f32, !llvm.ptr) -> ()
    "llvm.return"() : () -> ()
  }
}

// CHECK-LABEL: builtin.module {
// CHECK-NEXT:  llvm.func @refined_copy(%0: i64, %1: !llvm.ptr) {
// CHECK-NEXT:%2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK-NEXT:%3 = "llvm.getelementptr"(%1, %2) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK-NEXT:%4 = llvm.load %3 : !llvm.ptr -> f32
// CHECK-NEXT:%5 = "llvm.getelementptr"(%1, %2) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK-NEXT:"llvm.store"(%4, %5) : (f32, !llvm.ptr) -> ()
// CHECK-NEXT:"llvm.return"() : () -> ()
// CHECK-NEXT:  }
// CHECK-NEXT:}

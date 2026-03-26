// RUN: scair-opt %s -p lower-dmemref-to-llvm | filecheck %s -dump-input=always

builtin.module {
  func.func @semi_affine_layout_map(%stride0 : index, %stride1 : index) -> f32 {

    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %total = "arith.muli"(%c256 , %stride0): (index, index) -> index
    %flat_nat = "dtensor.index_to_nat"(%total) : (index) -> !dtensor.nat
    %flat = d_memref.alloc : () -> !d_memref.memref<[%flat_nat], f32>

    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index

    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
    
    %buf = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%flat_nat], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %zero, strides: [%stride0, %stride1]>
    
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %result = d_affine.for %i = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c256) step 1 : i32 iter_args(%acc = %cst : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c1024) step 1 : i32 iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %buf[%i, %j] : !d_memref.memref<[%d0, %d1], f32, offset: %zero, strides: [%stride0, %stride1]> -> f32
        %sum = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %sum : (f32)
      }
      d_affine.yield %inner : (f32)
    }
    d_memref.dealloc %flat : !d_memref.memref<[%flat_nat], f32>
    func.return %result : f32
  }
}

// CHECK-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// CHECK-NEXT:    %2 = llvm.mlir.constant 0 : index : index
// CHECK-NEXT:    %3 = "llvm.add"(%0, %2) : (index, index) -> index
// CHECK-NEXT:    %4 = "llvm.add"(%1, %2) : (index, index) -> index
// CHECK-NEXT:    %5 = llvm.mlir.constant 1 : index : index
// CHECK-NEXT:    %6 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %7 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %8 = llvm.mlir.constant 0 : index : index
// CHECK-NEXT:    %9 = llvm.mlir.constant 0.0 : f32 : f32
// CHECK-NEXT:    %10 = "llvm.mul"(%6, %3) : (index, index) -> index
// CHECK-NEXT:    %11 = llvm.mlir.zero : !llvm.ptr
// CHECK-NEXT:    %12 = "llvm.getelementptr"(%11, %10) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %13 = "llvm.ptrtoint"(%12) : (!llvm.ptr) -> index
// CHECK-NEXT:    %14 = "llvm.call"(%13) <{callee = @malloc}> : (index) -> !llvm.ptr
// CHECK-NEXT:    "llvm.br"(%8, %9)[^bb0] : (index, f32) -> ()
// CHECK-NEXT: ^bb0(%15: index, %16: f32):
// CHECK-NEXT:    %17 = "llvm.icmp"(%15, %6) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%17)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// CHECK-NEXT: ^bb1:
// CHECK-NEXT:    "llvm.br"(%15, %8, %16)[^bb3] : (index, index, f32) -> ()
// CHECK-NEXT: ^bb3(%18: index, %19: index, %20: f32):
// CHECK-NEXT:    %21 = "llvm.icmp"(%19, %7) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%21)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// CHECK-NEXT: ^bb4:
// CHECK-NEXT:    %22 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %23 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %24 = "llvm.mul"(%18, %3) : (index, index) -> index
// CHECK-NEXT:    %25 = "llvm.mul"(%19, %4) : (index, index) -> index
// CHECK-NEXT:    %26 = "llvm.add"(%24, %25) : (index, index) -> index
// CHECK-NEXT:    %27 = "llvm.getelementptr"(%14, %26) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %28 = llvm.load %27 : !llvm.ptr -> f32
// CHECK-NEXT:    %29 = "llvm.fadd"(%20, %28) : (f32, f32) -> f32
// CHECK-NEXT:    %30 = "llvm.add"(%19, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%18, %30, %29)[^bb3] : (index, index, f32) -> ()
// CHECK-NEXT: ^bb5:
// CHECK-NEXT:    %31 = "llvm.add"(%18, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%31, %20)[^bb0] : (index, f32) -> ()
// CHECK-NEXT: ^bb2:
// CHECK-NEXT:    "llvm.call"(%14) <{callee = @free}> : (!llvm.ptr) -> ()
// CHECK-NEXT:    "llvm.return"(%16) : (f32) -> ()
// CHECK-NEXT: }

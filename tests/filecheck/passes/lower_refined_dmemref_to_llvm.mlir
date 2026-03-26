// RUN: scair-opt %s -p lower-dmemref-to-llvm | filecheck %s -dump-input=always

builtin.module {
  func.func @semi_affine_layout_map(%stride0 : index, %stride1 : index) -> f32 {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32

    %total = "arith.muli"(%c256, %stride0) : (index, index) -> index
    %flat_nat = "dtensor.index_to_nat"(%total) : (index) -> !dtensor.nat
    %flat = d_memref.alloc : () -> !d_memref.memref<[%flat_nat], f32>

    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat

    %buf = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%flat_nat], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %zero, strides: [%stride0, %stride1]>

    d_affine.for %i = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c256) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index {
        d_memref.store %f1, %buf[%i, %j] : f32, !d_memref.memref<[%d0, %d1], f32, offset: %zero, strides: [%stride0, %stride1]>
        d_affine.yield
      }
      d_affine.yield
    }

    %result = d_affine.for %i = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c256) step 1 : index iter_args(%acc = %f0 : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index iter_args(%acc2 = %acc : f32) {
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
// CHECK-NEXT:    %2 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %3 = "llvm.add"(%0, %2) : (index, index) -> index
// CHECK-NEXT:    %4 = "llvm.add"(%1, %2) : (index, index) -> index
// CHECK-NEXT:    %5 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
// CHECK-NEXT:    %6 = "llvm.mlir.constant"() <{value = 256 : index}> : () -> index
// CHECK-NEXT:    %7 = "llvm.mlir.constant"() <{value = 1024 : index}> : () -> index
// CHECK-NEXT:    %8 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:    %9 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// CHECK-NEXT:    %10 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// CHECK-NEXT:    %11 = "llvm.mul"(%6, %3) : (index, index) -> index
// CHECK-NEXT:    %12 = "llvm.mlir.zero"() : () -> !llvm.ptr
// CHECK-NEXT:    %13 = "llvm.getelementptr"(%12, %11) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %14 = "llvm.ptrtoint"(%13) : (!llvm.ptr) -> index
// CHECK-NEXT:    %15 = "llvm.call"(%14) <{callee = @malloc}> : (index) -> !llvm.ptr
// CHECK-NEXT:    "llvm.br"(%8)[^bb0] : (index) -> ()
// CHECK-NEXT:  ^bb0(%16: index):
// CHECK-NEXT:    %17 = "llvm.icmp"(%16, %6) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%17)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// CHECK-NEXT:  ^bb1:
// CHECK-NEXT:    %18 = "llvm.mul"(%16, %3) : (index, index) -> index
// CHECK-NEXT:    %19 = "llvm.add"(%8, %18) : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%8)[^bb3] : (index) -> ()
// CHECK-NEXT:  ^bb2:
// CHECK-NEXT:    "llvm.br"(%8, %9)[^bb4] : (index, f32) -> ()
// CHECK-NEXT:  ^bb3(%20: index):
// CHECK-NEXT:    %21 = "llvm.icmp"(%20, %7) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%21)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// CHECK-NEXT:  ^bb5:
// CHECK-NEXT:    %22 = "llvm.mul"(%20, %4) : (index, index) -> index
// CHECK-NEXT:    %23 = "llvm.add"(%19, %22) : (index, index) -> index
// CHECK-NEXT:    %24 = "llvm.getelementptr"(%15, %23) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    "llvm.store"(%10, %24) : (f32, !llvm.ptr) -> ()
// CHECK-NEXT:    %25 = "llvm.add"(%20, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%25)[^bb3] : (index) -> ()
// CHECK-NEXT:  ^bb6:
// CHECK-NEXT:    %26 = "llvm.add"(%16, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%26)[^bb0] : (index) -> ()
// CHECK-NEXT:  ^bb4(%27: index, %28: f32):
// CHECK-NEXT:    %29 = "llvm.icmp"(%27, %6) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%29)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// CHECK-NEXT:  ^bb7:
// CHECK-NEXT:    %30 = "llvm.mul"(%27, %3) : (index, index) -> index
// CHECK-NEXT:    %31 = "llvm.add"(%8, %30) : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%27, %8, %28, %30, %31)[^bb9] : (index, index, f32, index, index) -> ()
// CHECK-NEXT:  ^bb9(%32: index, %33: index, %34: f32, %35: index, %36: index):
// CHECK-NEXT:    %37 = "llvm.icmp"(%33, %7) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%37)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// CHECK-NEXT:  ^bb10:
// CHECK-NEXT:    %38 = "llvm.mul"(%33, %4) : (index, index) -> index
// CHECK-NEXT:    %39 = "llvm.add"(%36, %38) : (index, index) -> index
// CHECK-NEXT:    %40 = "llvm.getelementptr"(%15, %39) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %41 = llvm.load %40 : !llvm.ptr -> f32
// CHECK-NEXT:    %42 = "llvm.fadd"(%34, %41) : (f32, f32) -> f32
// CHECK-NEXT:    %43 = "llvm.add"(%33, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%32, %43, %42, %35, %36)[^bb9] : (index, index, f32, index, index) -> ()
// CHECK-NEXT:  ^bb11:
// CHECK-NEXT:    %44 = "llvm.add"(%32, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%44, %34)[^bb4] : (index, f32) -> ()
// CHECK-NEXT:  ^bb8:
// CHECK-NEXT:    "llvm.call"(%15) <{callee = @free}> : (!llvm.ptr) -> ()
// CHECK-NEXT:    "llvm.return"(%28) : (f32) -> ()
// CHECK-NEXT: }

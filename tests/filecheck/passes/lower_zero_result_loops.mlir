// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm-baseline | filecheck %s --check-prefix=BASE
// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm | filecheck %s --check-prefix=OPT

builtin.module {
  func.func @zero_result_nested(%stride0 : index, %stride1 : index) {
    %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32

    %total = "arith.muli"(%c8, %stride0) : (index, index) -> index
    %flat = "memref.alloc"(%total) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> memref<?xf32>
    %A = "memref.reinterpret_cast"(%flat, %c0, %c8, %c8, %stride0, %stride1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<8x8xf32, strided<[?, ?], offset: 0>>

    "affine.for"(%c0, %c8) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 0>
    }> ({
    ^bb0(%i: index):
      "affine.for"(%c0, %c8) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 0>
      }> ({
      ^bb0(%j: index):
        "memref.store"(%f1, %A, %i, %j)
          : (f32, memref<8x8xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
        "affine.yield"() : () -> ()
      }) : (index, index) -> ()
      "affine.yield"() : () -> ()
    }) : (index, index) -> ()

    "memref.dealloc"(%flat) : (memref<?xf32>) -> ()
    "func.return"() : () -> ()
  }

  func.func @zero_then_reduction(%stride0 : index, %stride1 : index) -> f32 {
    %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32

    %total = "arith.muli"(%c8, %stride0) : (index, index) -> index
    %flat = "memref.alloc"(%total) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> memref<?xf32>
    %A = "memref.reinterpret_cast"(%flat, %c0, %c8, %c8, %stride0, %stride1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<8x8xf32, strided<[?, ?], offset: 0>>

    "affine.for"(%c0, %c8) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 0>
    }> ({
    ^bb0(%i: index):
      "affine.for"(%c0, %c8) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 0>
      }> ({
      ^bb0(%j: index):
        "memref.store"(%f1, %A, %i, %j)
          : (f32, memref<8x8xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
        "affine.yield"() : () -> ()
      }) : (index, index) -> ()
      "affine.yield"() : () -> ()
    }) : (index, index) -> ()

    %sum = "affine.for"(%c0, %c8, %f0) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 1>
    }> ({
    ^bb0(%i: index, %acc: f32):
      %inner = "affine.for"(%c0, %c8, %acc) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 1>
      }> ({
      ^bb0(%j: index, %acc2: f32):
        %x = "memref.load"(%A, %i, %j)
          : (memref<8x8xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
        %y = "arith.addf"(%acc2, %x) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        "affine.yield"(%y) : (f32) -> ()
      }) : (index, index, f32) -> f32
      "affine.yield"(%inner) : (f32) -> ()
    }) : (index, index, f32) -> f32

    "memref.dealloc"(%flat) : (memref<?xf32>) -> ()
    "func.return"(%sum) : (f32) -> ()
  }
}

// -----------------------------------------------------------------------------
// Baseline dynamic descriptor lowering keeps descriptor traffic, but the loop
// CFG should still use only real loop-carried block arguments.
// -----------------------------------------------------------------------------

// BASE-LABEL: func.func @zero_result_nested(%0: index, %1: index) {
// BASE: %2 = llvm.mlir.constant 8 : index : index
// BASE: %3 = llvm.mlir.constant 0 : index : index
// BASE: %4 = llvm.mlir.constant 1 : index : index
// BASE: %5 = llvm.mlir.constant 1.0 : f32 : f32
// BASE: %6 = "llvm.mul"(%2, %0) : (index, index) -> index
// BASE: "llvm.br"(%3)[^bb0] : (index) -> ()
// BASE: ^bb0(%27: index):
// BASE: %28 = "llvm.icmp"(%27, %2) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%28)[^bb1, ^bb2]
// BASE: ^bb1:
// BASE: "llvm.br"(%3)[^bb3] : (index) -> ()
// BASE: ^bb3(%30: index):
// BASE: %31 = "llvm.icmp"(%30, %2) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%31)[^bb4, ^bb5]
// BASE: ^bb4:
// BASE: %32 = "llvm.extractvalue"(%26) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// BASE: %33 = "llvm.extractvalue"(%26) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// BASE: %34 = "llvm.mul"(%27, %33) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: %35 = "llvm.extractvalue"(%26) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// BASE: %36 = "llvm.mul"(%30, %35) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: %37 = "llvm.add"(%34, %36) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: %38 = "llvm.getelementptr"(%32, %37) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// BASE: llvm.store %5, %38 : f32, !llvm.ptr
// BASE: %39 = llvm.mlir.constant 1 : index : index
// BASE: %40 = "llvm.add"(%30, %39) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%40)[^bb3] : (index) -> ()
// BASE: ^bb5:
// BASE: %41 = llvm.mlir.constant 1 : index : index
// BASE: %42 = "llvm.add"(%27, %41) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%42)[^bb0] : (index) -> ()

// BASE-LABEL: func.func @zero_then_reduction(%0: index, %1: index) -> f32 {
// BASE: %2 = llvm.mlir.constant 8 : index : index
// BASE: %3 = llvm.mlir.constant 0 : index : index
// BASE: %4 = llvm.mlir.constant 1 : index : index
// BASE: %5 = llvm.mlir.constant 0.0 : f32 : f32
// BASE: %6 = llvm.mlir.constant 1.0 : f32 : f32
// BASE: %7 = "llvm.mul"(%2, %0) : (index, index) -> index
// BASE: "llvm.br"(%3)[^bb0] : (index) -> ()
// BASE: ^bb0(%28: index):
// BASE: %29 = "llvm.icmp"(%28, %2) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%29)[^bb1, ^bb2]
// BASE: ^bb1:
// BASE: "llvm.br"(%3)[^bb3] : (index) -> ()
// BASE: ^bb3(%30: index):
// BASE: %31 = "llvm.icmp"(%30, %2) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%31)[^bb5, ^bb6]
// BASE: ^bb5:
// BASE: %32 = "llvm.extractvalue"(%27) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// BASE: %33 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// BASE: %34 = "llvm.mul"(%28, %33) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: %35 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// BASE: %36 = "llvm.mul"(%30, %35) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: %37 = "llvm.add"(%34, %36) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: %38 = "llvm.getelementptr"(%32, %37) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// BASE: llvm.store %6, %38 : f32, !llvm.ptr
// BASE: %39 = llvm.mlir.constant 1 : index : index
// BASE: %40 = "llvm.add"(%30, %39) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%40)[^bb3] : (index) -> ()
// BASE: ^bb4(%43: index, %44: f32):
// BASE: %45 = "llvm.icmp"(%43, %2) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%45)[^bb7, ^bb8]
// BASE: ^bb7:
// BASE: "llvm.br"(%3, %44)[^bb9] : (index, f32) -> ()
// BASE: ^bb9(%46: index, %47: f32):
// BASE: %48 = "llvm.icmp"(%46, %2) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%48)[^bb10, ^bb11]
// BASE: ^bb10:
// BASE: %49 = "llvm.extractvalue"(%27) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// BASE: %50 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// BASE: %51 = "llvm.mul"(%43, %50) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: %52 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// BASE: %53 = "llvm.mul"(%46, %52) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: %54 = "llvm.add"(%51, %53) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: %55 = "llvm.getelementptr"(%49, %54) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// BASE: %56 = llvm.load %55 : !llvm.ptr -> f32
// BASE: %57 = "llvm.fadd"(%47, %56) : (f32, f32) -> f32
// BASE: %58 = llvm.mlir.constant 1 : index : index
// BASE: %59 = "llvm.add"(%46, %58) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%59, %57)[^bb9] : (index, f32) -> ()

// -----------------------------------------------------------------------------
// Dynamic -> refined -> pointer lowering should use the same minimal CFG shape
// without descriptors, with explicit GEP/store and GEP/load/fadd in the loops.
// -----------------------------------------------------------------------------

// OPT-LABEL: func.func @zero_result_nested(%0: index, %1: index) {
// OPT: %2 = llvm.mlir.constant 0 : index : index
// OPT: %3 = "llvm.add"(%0, %2) : (index, index) -> index
// OPT: %4 = "llvm.add"(%1, %2) : (index, index) -> index
// OPT: %5 = llvm.mlir.constant 1 : index : index
// OPT: %6 = llvm.mlir.constant 8 : index : index
// OPT: %7 = llvm.mlir.constant 0 : index : index
// OPT: %8 = llvm.mlir.constant 1.0 : f32 : f32
// OPT: "llvm.br"(%7)[^bb0] : (index) -> ()
// OPT: ^bb0(%14: index):
// OPT: %15 = "llvm.icmp"(%14, %6) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%15)[^bb1, ^bb2]
// OPT: ^bb1:
// OPT: %16 = "llvm.mul"(%14, %3) : (index, index) -> index
// OPT: %17 = "llvm.add"(%7, %16) : (index, index) -> index
// OPT: "llvm.br"(%7)[^bb3] : (index) -> ()
// OPT: ^bb3(%18: index):
// OPT: %19 = "llvm.icmp"(%18, %6) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%19)[^bb4, ^bb5]
// OPT: ^bb4:
// OPT: %20 = "llvm.mul"(%18, %4) : (index, index) -> index
// OPT: %21 = "llvm.add"(%17, %20) : (index, index) -> index
// OPT: %22 = "llvm.getelementptr"(%13, %21) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// OPT: llvm.store %8, %22 : f32, !llvm.ptr
// OPT: %23 = "llvm.add"(%18, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%23)[^bb3] : (index) -> ()
// OPT: ^bb5:
// OPT: %24 = "llvm.add"(%14, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%24)[^bb0] : (index) -> ()

// OPT-LABEL: func.func @zero_then_reduction(%0: index, %1: index) -> f32 {
// OPT: %2 = llvm.mlir.constant 0 : index : index
// OPT: %3 = "llvm.add"(%0, %2) : (index, index) -> index
// OPT: %4 = "llvm.add"(%1, %2) : (index, index) -> index
// OPT: %5 = llvm.mlir.constant 1 : index : index
// OPT: %6 = llvm.mlir.constant 8 : index : index
// OPT: %7 = llvm.mlir.constant 0 : index : index
// OPT: %8 = llvm.mlir.constant 0.0 : f32 : f32
// OPT: %9 = llvm.mlir.constant 1.0 : f32 : f32
// OPT: "llvm.br"(%7)[^bb0] : (index) -> ()
// OPT: ^bb0(%15: index):
// OPT: %16 = "llvm.icmp"(%15, %6) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%16)[^bb1, ^bb2]
// OPT: ^bb1:
// OPT: %17 = "llvm.mul"(%15, %3) : (index, index) -> index
// OPT: %18 = "llvm.add"(%7, %17) : (index, index) -> index
// OPT: "llvm.br"(%7)[^bb3] : (index) -> ()
// OPT: ^bb3(%19: index):
// OPT: %20 = "llvm.icmp"(%19, %6) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%20)[^bb5, ^bb6]
// OPT: ^bb5:
// OPT: %21 = "llvm.mul"(%19, %4) : (index, index) -> index
// OPT: %22 = "llvm.add"(%18, %21) : (index, index) -> index
// OPT: %23 = "llvm.getelementptr"(%14, %22) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// OPT: llvm.store %9, %23 : f32, !llvm.ptr
// OPT: %24 = "llvm.add"(%19, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%24)[^bb3] : (index) -> ()
// OPT: ^bb4(%26: index, %27: f32):
// OPT: %28 = "llvm.icmp"(%26, %6) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%28)[^bb7, ^bb8]
// OPT: ^bb7:
// OPT: %29 = "llvm.mul"(%26, %3) : (index, index) -> index
// OPT: %30 = "llvm.add"(%7, %29) : (index, index) -> index
// OPT: "llvm.br"(%26, %7, %27, %29, %30)[^bb9] : (index, index, f32, index, index) -> ()
// OPT: ^bb9(%31: index, %32: index, %33: f32, %34: index, %35: index):
// OPT: %36 = "llvm.icmp"(%32, %6) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%36)[^bb10, ^bb11]
// OPT: ^bb10:
// OPT: %37 = "llvm.mul"(%32, %4) : (index, index) -> index
// OPT: %38 = "llvm.add"(%35, %37) : (index, index) -> index
// OPT: %39 = "llvm.getelementptr"(%14, %38) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// OPT: %40 = llvm.load %39 : !llvm.ptr -> f32
// OPT: %41 = "llvm.fadd"(%33, %40) : (f32, f32) -> f32
// OPT: %42 = "llvm.add"(%32, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%31, %42, %41, %34, %35)[^bb9] : (index, index, f32, index, index) -> ()

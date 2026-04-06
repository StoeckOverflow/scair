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

// BASE-LABEL: func.func @zero_result_nested(%0: i64, %1: i64) {
// BASE: %2 = "llvm.mlir.constant"() <{value = 8}> : () -> i64
// BASE: %3 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// BASE: %4 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// BASE: %5 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// BASE: %6 = "llvm.mul"(%2, %0) : (i64, i64) -> i64
// BASE: "llvm.br"(%3)[^bb0] : (i64) -> ()
// BASE: ^bb0(%27: i64):
// BASE: %28 = llvm.icmp "slt" %27, %2 : i64
// BASE: "llvm.cond_br"(%28)[^bb1, ^bb2]
// BASE: ^bb1:
// BASE: "llvm.br"(%3)[^bb3] : (i64) -> ()
// BASE: ^bb3(%30: i64):
// BASE: %31 = llvm.icmp "slt" %30, %2 : i64
// BASE: "llvm.cond_br"(%31)[^bb4, ^bb5]
// BASE: ^bb4:
// BASE: %32 = llvm.extractvalue %26[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// BASE: %33 = llvm.extractvalue %26[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// BASE: %34 = "llvm.mul"(%27, %33) : (i64, i64) -> i64
// BASE: %35 = llvm.extractvalue %26[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// BASE: %36 = "llvm.mul"(%30, %35) : (i64, i64) -> i64
// BASE: %37 = "llvm.add"(%34, %36) : (i64, i64) -> i64
// BASE: %38 = "llvm.getelementptr"(%32, %37) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// BASE: "llvm.store"(%5, %38) : (f32, !llvm.ptr) -> ()
// BASE: %39 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// BASE: %40 = "llvm.add"(%30, %39) : (i64, i64) -> i64
// BASE: "llvm.br"(%40)[^bb3] : (i64) -> ()
// BASE: ^bb5:
// BASE: %41 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// BASE: %42 = "llvm.add"(%27, %41) : (i64, i64) -> i64
// BASE: "llvm.br"(%42)[^bb0] : (i64) -> ()

// BASE-LABEL: func.func @zero_then_reduction(%0: i64, %1: i64) -> f32 {
// BASE: %2 = "llvm.mlir.constant"() <{value = 8}> : () -> i64
// BASE: %3 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// BASE: %4 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// BASE: %5 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// BASE: %6 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// BASE: %7 = "llvm.mul"(%2, %0) : (i64, i64) -> i64
// BASE: "llvm.br"(%3)[^bb0] : (i64) -> ()
// BASE: ^bb0(%28: i64):
// BASE: %29 = llvm.icmp "slt" %28, %2 : i64
// BASE: "llvm.cond_br"(%29)[^bb1, ^bb2]
// BASE: ^bb1:
// BASE: "llvm.br"(%3)[^bb3] : (i64) -> ()
// BASE: ^bb3(%30: i64):
// BASE: %31 = llvm.icmp "slt" %30, %2 : i64
// BASE: "llvm.cond_br"(%31)[^bb5, ^bb6]
// BASE: ^bb5:
// BASE: %32 = llvm.extractvalue %27[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// BASE: %33 = llvm.extractvalue %27[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// BASE: %34 = "llvm.mul"(%28, %33) : (i64, i64) -> i64
// BASE: %35 = llvm.extractvalue %27[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// BASE: %36 = "llvm.mul"(%30, %35) : (i64, i64) -> i64
// BASE: %37 = "llvm.add"(%34, %36) : (i64, i64) -> i64
// BASE: %38 = "llvm.getelementptr"(%32, %37) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// BASE: "llvm.store"(%6, %38) : (f32, !llvm.ptr) -> ()
// BASE: %39 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// BASE: %40 = "llvm.add"(%30, %39) : (i64, i64) -> i64
// BASE: "llvm.br"(%40)[^bb3] : (i64) -> ()
// BASE: ^bb4(%43: i64, %44: f32):
// BASE: %45 = llvm.icmp "slt" %43, %2 : i64
// BASE: "llvm.cond_br"(%45)[^bb7, ^bb8]
// BASE: ^bb7:
// BASE: "llvm.br"(%3, %44)[^bb9] : (i64, f32) -> ()
// BASE: ^bb9(%46: i64, %47: f32):
// BASE: %48 = llvm.icmp "slt" %46, %2 : i64
// BASE: "llvm.cond_br"(%48)[^bb10, ^bb11]
// BASE: ^bb10:
// BASE: %49 = llvm.extractvalue %27[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// BASE: %50 = llvm.extractvalue %27[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// BASE: %51 = "llvm.mul"(%43, %50) : (i64, i64) -> i64
// BASE: %52 = llvm.extractvalue %27[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// BASE: %53 = "llvm.mul"(%46, %52) : (i64, i64) -> i64
// BASE: %54 = "llvm.add"(%51, %53) : (i64, i64) -> i64
// BASE: %55 = "llvm.getelementptr"(%49, %54) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// BASE: %56 = llvm.load %55 : !llvm.ptr -> f32
// BASE: %57 = "llvm.fadd"(%47, %56) : (f32, f32) -> f32
// BASE: %58 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// BASE: %59 = "llvm.add"(%46, %58) : (i64, i64) -> i64
// BASE: "llvm.br"(%59, %57)[^bb9] : (i64, f32) -> ()

// -----------------------------------------------------------------------------
// Dynamic -> refined -> pointer lowering should use the same minimal CFG shape
// without descriptors, with explicit GEP/store and GEP/load/fadd in the loops.
// -----------------------------------------------------------------------------

// OPT-LABEL: func.func @zero_result_nested(%0: i64, %1: i64) {
// OPT: %2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// OPT: %3 = "llvm.add"(%0, %2) : (i64, i64) -> i64
// OPT: %4 = "llvm.add"(%1, %2) : (i64, i64) -> i64
// OPT: %5 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// OPT: %6 = "llvm.mlir.constant"() <{value = 8}> : () -> i64
// OPT: %7 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// OPT: %8 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// OPT: "llvm.br"(%7)[^bb0] : (i64) -> ()
// OPT: ^bb0(%14: i64):
// OPT: %15 = llvm.icmp "slt" %14, %6 : i64
// OPT: "llvm.cond_br"(%15)[^bb1, ^bb2]
// OPT: ^bb1:
// OPT: "llvm.br"(%7)[^bb3] : (i64) -> ()
// OPT: ^bb3(%16: i64):
// OPT: %17 = llvm.icmp "slt" %16, %6 : i64
// OPT: "llvm.cond_br"(%17)[^bb4, ^bb5]
// OPT: ^bb4:
// OPT: %18 = "llvm.mul"(%14, %3) : (i64, i64) -> i64
// OPT: %19 = "llvm.mul"(%16, %4) : (i64, i64) -> i64
// OPT: %20 = "llvm.add"(%7, %18) : (i64, i64) -> i64
// OPT: %21 = "llvm.add"(%20, %19) : (i64, i64) -> i64
// OPT: %22 = "llvm.getelementptr"(%13, %21) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// OPT: "llvm.store"(%8, %22) : (f32, !llvm.ptr) -> ()
// OPT: %23 = "llvm.add"(%16, %5) : (i64, i64) -> i64
// OPT: "llvm.br"(%23)[^bb3] : (i64) -> ()
// OPT: ^bb5:
// OPT: %24 = "llvm.add"(%14, %5) : (i64, i64) -> i64
// OPT: "llvm.br"(%24)[^bb0] : (i64) -> ()

// OPT-LABEL: func.func @zero_then_reduction(%0: i64, %1: i64) -> f32 {
// OPT: %2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// OPT: %3 = "llvm.add"(%0, %2) : (i64, i64) -> i64
// OPT: %4 = "llvm.add"(%1, %2) : (i64, i64) -> i64
// OPT: %5 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// OPT: %6 = "llvm.mlir.constant"() <{value = 8}> : () -> i64
// OPT: %7 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// OPT: %8 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// OPT: %9 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// OPT: "llvm.br"(%7)[^bb0] : (i64) -> ()
// OPT: ^bb0(%15: i64):
// OPT: %16 = llvm.icmp "slt" %15, %6 : i64
// OPT: "llvm.cond_br"(%16)[^bb1, ^bb2]
// OPT: ^bb1:
// OPT: "llvm.br"(%7)[^bb3] : (i64) -> ()
// OPT: ^bb3(%17: i64):
// OPT: %18 = llvm.icmp "slt" %17, %6 : i64
// OPT: "llvm.cond_br"(%18)[^bb5, ^bb6]
// OPT: ^bb5:
// OPT: %19 = "llvm.mul"(%15, %3) : (i64, i64) -> i64
// OPT: %20 = "llvm.mul"(%17, %4) : (i64, i64) -> i64
// OPT: %21 = "llvm.add"(%7, %19) : (i64, i64) -> i64
// OPT: %22 = "llvm.add"(%21, %20) : (i64, i64) -> i64
// OPT: %23 = "llvm.getelementptr"(%14, %22) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// OPT: "llvm.store"(%9, %23) : (f32, !llvm.ptr) -> ()
// OPT: %24 = "llvm.add"(%17, %5) : (i64, i64) -> i64
// OPT: "llvm.br"(%24)[^bb3] : (i64) -> ()
// OPT: ^bb4(%26: i64, %27: f32):
// OPT: %28 = llvm.icmp "slt" %26, %6 : i64
// OPT: "llvm.cond_br"(%28)[^bb7, ^bb8]
// OPT: ^bb7:
// OPT: "llvm.br"(%26, %7, %27)[^bb9] : (i64, i64, f32) -> ()
// OPT: ^bb9(%29: i64, %30: i64, %31: f32):
// OPT: %32 = llvm.icmp "slt" %30, %6 : i64
// OPT: "llvm.cond_br"(%32)[^bb10, ^bb11]
// OPT: ^bb10:
// OPT: %33 = "llvm.mul"(%29, %3) : (i64, i64) -> i64
// OPT: %34 = "llvm.mul"(%30, %4) : (i64, i64) -> i64
// OPT: %35 = "llvm.add"(%7, %33) : (i64, i64) -> i64
// OPT: %36 = "llvm.add"(%35, %34) : (i64, i64) -> i64
// OPT: %37 = "llvm.getelementptr"(%14, %36) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// OPT: %38 = llvm.load %37 : !llvm.ptr -> f32
// OPT: %39 = "llvm.fadd"(%31, %38) : (f32, f32) -> f32
// OPT: %40 = "llvm.add"(%30, %5) : (i64, i64) -> i64
// OPT: "llvm.br"(%29, %40, %39)[^bb9] : (i64, i64, f32) -> ()

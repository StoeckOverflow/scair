// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm-baseline | filecheck %s --check-prefix=P1
// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm | filecheck %s --check-prefix=P2
// RUN: scair-opt %S/lower_refined_dmemref_to_llvm.mlir -p lower-dmemref-to-llvm | filecheck %s --check-prefix=P3


// builtin.module {
//   func.func @semi_affine_layout_map(%stride0 : index, %stride1 : index) -> f32 {
//     %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
//     %total = "arith.muli"(%c256, %stride0) : (index, index) -> index
//     %flat = "memref.alloc"(%total) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> memref<?xf32>
//     %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
//     %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
//     %buf = "memref.reinterpret_cast"(%flat, %c0, %c256, %c1024, %stride0, %stride1) <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}> : (memref<?xf32>, index, index, index, index, index) -> memref<256x1024xf32, strided<[?, ?], offset: 0>>
//     %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
//     %result = "affine.for"(%c0, %c256, %cst) <{lowerBoundMap = affine_map<(d0) -> (d0)>, upperBoundMap = affine_map<(d0) -> (d0)>, step = 1 : index, operandSegmentSizes = array<i32: 1, 1, 1>}> ({
//     ^bb0(%i: index, %acc: f32):
//       %inner = "affine.for"(%c0, %c1024, %acc) <{lowerBoundMap = affine_map<(d0) -> (d0)>, upperBoundMap = affine_map<(d0) -> (d0)>, step = 1 : index, operandSegmentSizes = array<i32: 1, 1, 1>}> ({
//       ^bb0(%j: index, %acc2: f32):
//         %v = "memref.load"(%buf, %i, %j) : (memref<256x1024xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
//         %sum = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
//         "affine.yield"(%sum) : (f32) -> ()
//       }) : (index, index, f32) -> f32
//       "affine.yield"(%inner) : (f32) -> ()
//     }) : (index, index, f32) -> f32
//     memref.dealloc %flat : memref<?xf32>
//     func.return %result : f32
//   }
// }


builtin.module {
  func.func @semi_affine_layout_map(%stride0 : index, %stride1 : index) -> f32 {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1_i32 = "arith.constant"() <{value = 0 : i32}> : () -> i32
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32

    %total = "arith.muli"(%c256, %stride0) : (index, index) -> index
    %flat = "memref.alloc"(%total) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> memref<?xf32>

    %buf = "memref.reinterpret_cast"(%flat, %c0, %c256, %c1024, %stride0, %stride1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<256x1024xf32, strided<[?, ?], offset: 0>>

    // Initialize every element to 1.0
    "affine.for"(%c0, %c256) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 0>
    }> ({
    ^bb0(%i: index):
      "affine.for"(%c0, %c1024) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 0>
      }> ({
      ^bb0(%j: index):
        "memref.store"(%f1, %buf, %i, %j)
          : (f32, memref<256x1024xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
        "affine.yield"() : () -> ()
      }) : (index, index) -> ()
      "affine.yield"() : () -> ()
    }) : (index, index) -> ()

    // Sum all elements
    %result = "affine.for"(%c0, %c256, %f0) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 1>
    }> ({
    ^bb0(%i: index, %acc: f32):
      %inner = "affine.for"(%c0, %c1024, %acc) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 1>
      }> ({
      ^bb0(%j: index, %acc2: f32):
        %v = "memref.load"(%buf, %i, %j)
          : (memref<256x1024xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
        %sum = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        "affine.yield"(%sum) : (f32) -> ()
      }) : (index, index, f32) -> f32
      "affine.yield"(%inner) : (f32) -> ()
    }) : (index, index, f32) -> f32

    "memref.dealloc"(%flat) : (memref<?xf32>) -> ()
    "func.return"(%result) : (f32) -> ()
  }

  func.func private @printF32(f32)
  func.func private @printNewline()

  func.func @main() -> i32 {
    %stride0 = "arith.constant"() <{value = 256 : index}> : () -> index
    %stride1 = "arith.constant"() <{value = 1024 : index}> : () -> index
    
    %ret = "arith.constant"() <{value = 0 : index}> : () -> i32
    
    %res = "func.call"(%stride0, %stride1) <{"callee" = @semi_affine_layout_map}> 
    : (index, index) -> f32

    "func.call"(%res) <{"callee" = @printF32}> : (f32) -> ()
    "func.call"() <{"callee" = @printNewline}> : () -> ()

    func.return %ret : i32
  }
}

// P1-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// P1-NEXT:    %2 = "llvm.mlir.constant"() <{value = 256 : index}> : () -> index
// P1-NEXT:    %3 = "llvm.mlir.constant"() <{value = 1024 : index}> : () -> index
// P1-NEXT:    %4 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
// P1-NEXT:    %5 = "llvm.mlir.constant"() <{value = 0 : i32}> : () -> i32
// P1-NEXT:    %6 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P1-NEXT:    %7 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// P1-NEXT:    %8 = "llvm.mul"(%2, %0) : (index, index) -> index
// P1-NEXT:    %9 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
// P1-NEXT:    %10 = "llvm.mlir.zero"() : () -> !llvm.ptr
// P1-NEXT:    %11 = "llvm.getelementptr"(%10, %8) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P1-NEXT:    %12 = "llvm.ptrtoint"(%11) : (!llvm.ptr) -> index
// P1-NEXT:    %13 = "llvm.call"(%12) <{callee = @malloc}> : (index) -> !llvm.ptr
// P1-NEXT:    %14 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %15 = "llvm.insertvalue"(%13, %14) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %16 = "llvm.insertvalue"(%13, %15) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %17 = "llvm.insertvalue"(%4, %16) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %18 = "llvm.insertvalue"(%8, %17) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %19 = "llvm.insertvalue"(%9, %18) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %20 = "llvm.extractvalue"(%19) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P1-NEXT:    %21 = "llvm.extractvalue"(%19) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P1-NEXT:    %22 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %23 = "llvm.insertvalue"(%20, %22) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %24 = "llvm.insertvalue"(%21, %23) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %25 = "llvm.insertvalue"(%4, %24) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %26 = "llvm.insertvalue"(%2, %25) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %27 = "llvm.insertvalue"(%3, %26) <{position = array<i32: 3, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %28 = "llvm.insertvalue"(%0, %27) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %29 = "llvm.insertvalue"(%1, %28) <{position = array<i32: 4, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    "llvm.br"(%4)[^bb0] : (index) -> ()
// P1-NEXT:  ^bb0(%30: index):
// P1-NEXT:    %31 = "llvm.icmp"(%30, %2) <{predicate = "slt"}> : (index, index) -> i1
// P1-NEXT:    "llvm.cond_br"(%31)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P1-NEXT:  ^bb1:
// P1-NEXT:    "llvm.br"(%4)[^bb3] : (index) -> ()
// P1-NEXT:  ^bb2:
// P1-NEXT:    "llvm.br"(%4, %6)[^bb4] : (index, f32) -> ()
// P1-NEXT:  ^bb3(%32: index):
// P1-NEXT:    %33 = "llvm.icmp"(%32, %3) <{predicate = "slt"}> : (index, index) -> i1
// P1-NEXT:    "llvm.cond_br"(%33)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P1-NEXT:  ^bb5:
// P1-NEXT:    %34 = "llvm.extractvalue"(%29) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// P1-NEXT:    %35 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P1-NEXT:    %36 = "llvm.mul"(%30, %35) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %37 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P1-NEXT:    %38 = "llvm.mul"(%32, %37) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %39 = "llvm.add"(%36, %38) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %40 = "llvm.getelementptr"(%34, %39) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// P1-NEXT:    "llvm.store"(%7, %40) : (f32, !llvm.ptr) -> ()
// P1-NEXT:    %41 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
// P1-NEXT:    %42 = "llvm.add"(%32, %41) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%42)[^bb3] : (index) -> ()
// P1-NEXT:  ^bb6:
// P1-NEXT:    %43 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
// P1-NEXT:    %44 = "llvm.add"(%30, %43) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%44)[^bb0] : (index) -> ()
// P1-NEXT:  ^bb4(%45: index, %46: f32):
// P1-NEXT:    %47 = "llvm.icmp"(%45, %2) <{predicate = "slt"}> : (index, index) -> i1
// P1-NEXT:    "llvm.cond_br"(%47)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P1-NEXT:  ^bb7:
// P1-NEXT:    "llvm.br"(%4, %46)[^bb9] : (index, f32) -> ()
// P1-NEXT:  ^bb9(%48: index, %49: f32):
// P1-NEXT:    %50 = "llvm.icmp"(%48, %3) <{predicate = "slt"}> : (index, index) -> i1
// P1-NEXT:    "llvm.cond_br"(%50)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P1-NEXT:  ^bb10:
// P1-NEXT:    %51 = "llvm.extractvalue"(%29) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// P1-NEXT:    %52 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P1-NEXT:    %53 = "llvm.mul"(%45, %52) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %54 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P1-NEXT:    %55 = "llvm.mul"(%48, %54) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %56 = "llvm.add"(%53, %55) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %57 = "llvm.getelementptr"(%51, %56) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// P1-NEXT:    %58 = llvm.load %57 : !llvm.ptr -> f32
// P1-NEXT:    %59 = "llvm.fadd"(%49, %58) : (f32, f32) -> f32
// P1-NEXT:    %60 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
// P1-NEXT:    %61 = "llvm.add"(%48, %60) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%61, %59)[^bb9] : (index, f32) -> ()
// P1-NEXT:  ^bb11:
// P1-NEXT:    %62 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
// P1-NEXT:    %63 = "llvm.add"(%45, %62) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%63, %49)[^bb4] : (index, f32) -> ()
// P1-NEXT:  ^bb8:
// P1-NEXT:    %64 = "llvm.extractvalue"(%19) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P1-NEXT:    "llvm.call"(%64) <{callee = @free}> : (!llvm.ptr) -> ()
// P1-NEXT:    "llvm.return"(%46) : (f32) -> ()
// P1-NEXT:  }

// P2-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// P2-NEXT:    %2 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
// P2-NEXT:    %3 = "llvm.add"(%0, %2) : (index, index) -> index
// P2-NEXT:    %4 = "llvm.add"(%1, %2) : (index, index) -> index
// P2-NEXT:    %5 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
// P2-NEXT:    %6 = "llvm.mlir.constant"() <{value = 256 : index}> : () -> index
// P2-NEXT:    %7 = "llvm.mlir.constant"() <{value = 1024 : index}> : () -> index
// P2-NEXT:    %8 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
// P2-NEXT:    %9 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P2-NEXT:    %10 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// P2-NEXT:    %11 = "llvm.mul"(%6, %3) : (index, index) -> index
// P2-NEXT:    %12 = "llvm.mlir.zero"() : () -> !llvm.ptr
// P2-NEXT:    %13 = "llvm.getelementptr"(%12, %11) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P2-NEXT:    %14 = "llvm.ptrtoint"(%13) : (!llvm.ptr) -> index
// P2-NEXT:    %15 = "llvm.call"(%14) <{callee = @malloc}> : (index) -> !llvm.ptr
// P2-NEXT:    "llvm.br"(%8)[^bb0] : (index) -> ()
// P2-NEXT:  ^bb0(%16: index):
// P2-NEXT:    %17 = "llvm.icmp"(%16, %6) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%17)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P2-NEXT:  ^bb1:
// P2-NEXT:    %18 = "llvm.mul"(%16, %3) : (index, index) -> index
// P2-NEXT:    %19 = "llvm.add"(%8, %18) : (index, index) -> index
// P2-NEXT:    "llvm.br"(%8)[^bb3] : (index) -> ()
// P2-NEXT:  ^bb2:
// P2-NEXT:    "llvm.br"(%8, %9)[^bb4] : (index, f32) -> ()
// P2-NEXT:  ^bb3(%20: index):
// P2-NEXT:    %21 = "llvm.icmp"(%20, %7) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%21)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P2-NEXT:  ^bb5:
// P2-NEXT:    %22 = "llvm.mul"(%20, %4) : (index, index) -> index
// P2-NEXT:    %23 = "llvm.add"(%19, %22) : (index, index) -> index
// P2-NEXT:    %24 = "llvm.getelementptr"(%15, %23) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P2-NEXT:    "llvm.store"(%10, %24) : (f32, !llvm.ptr) -> ()
// P2-NEXT:    %25 = "llvm.add"(%20, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%25)[^bb3] : (index) -> ()
// P2-NEXT:  ^bb6:
// P2-NEXT:    %26 = "llvm.add"(%16, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%26)[^bb0] : (index) -> ()
// P2-NEXT:  ^bb4(%27: index, %28: f32):
// P2-NEXT:    %29 = "llvm.icmp"(%27, %6) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%29)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P2-NEXT:  ^bb7:
// P2-NEXT:    %30 = "llvm.mul"(%27, %3) : (index, index) -> index
// P2-NEXT:    %31 = "llvm.add"(%8, %30) : (index, index) -> index
// P2-NEXT:    "llvm.br"(%27, %8, %28, %30, %31)[^bb9] : (index, index, f32, index, index) -> ()
// P2-NEXT:  ^bb9(%32: index, %33: index, %34: f32, %35: index, %36: index):
// P2-NEXT:    %37 = "llvm.icmp"(%33, %7) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%37)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P2-NEXT:  ^bb10:
// P2-NEXT:    %38 = "llvm.mul"(%33, %4) : (index, index) -> index
// P2-NEXT:    %39 = "llvm.add"(%36, %38) : (index, index) -> index
// P2-NEXT:    %40 = "llvm.getelementptr"(%15, %39) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P2-NEXT:    %41 = llvm.load %40 : !llvm.ptr -> f32
// P2-NEXT:    %42 = "llvm.fadd"(%34, %41) : (f32, f32) -> f32
// P2-NEXT:    %43 = "llvm.add"(%33, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%32, %43, %42, %35, %36)[^bb9] : (index, index, f32, index, index) -> ()
// P2-NEXT:  ^bb11:
// P2-NEXT:    %44 = "llvm.add"(%32, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%44, %34)[^bb4] : (index, f32) -> ()
// P2-NEXT:  ^bb8:
// P2-NEXT:    "llvm.call"(%15) <{callee = @free}> : (!llvm.ptr) -> ()
// P2-NEXT:    "llvm.return"(%28) : (f32) -> ()
// P2-NEXT:  }

// P3-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// P3-NEXT:    %2 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
// P3-NEXT:    %3 = "llvm.add"(%0, %2) : (index, index) -> index
// P3-NEXT:    %4 = "llvm.add"(%1, %2) : (index, index) -> index
// P3-NEXT:    %5 = "llvm.mlir.constant"() <{value = 1 : index}> : () -> index
// P3-NEXT:    %6 = "llvm.mlir.constant"() <{value = 256 : index}> : () -> index
// P3-NEXT:    %7 = "llvm.mlir.constant"() <{value = 1024 : index}> : () -> index
// P3-NEXT:    %8 = "llvm.mlir.constant"() <{value = 0 : index}> : () -> index
// P3-NEXT:    %9 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P3-NEXT:    %10 = "llvm.mul"(%6, %3) : (index, index) -> index
// P3-NEXT:    %11 = "llvm.mlir.zero"() : () -> !llvm.ptr
// P3-NEXT:    %12 = "llvm.getelementptr"(%11, %10) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P3-NEXT:    %13 = "llvm.ptrtoint"(%12) : (!llvm.ptr) -> index
// P3-NEXT:    %14 = "llvm.call"(%13) <{callee = @malloc}> : (index) -> !llvm.ptr
// P3-NEXT:    "llvm.br"(%8, %9)[^bb0] : (index, f32) -> ()
// P3-NEXT:  ^bb0(%15: index, %16: f32):
// P3-NEXT:    %17 = "llvm.icmp"(%15, %6) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%17)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P3-NEXT:  ^bb1:
// P3-NEXT:    "llvm.br"(%15, %8, %16)[^bb3] : (index, index, f32) -> ()
// P3-NEXT:  ^bb3(%18: index, %19: index, %20: f32):
// P3-NEXT:    %21 = "llvm.icmp"(%19, %7) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%21)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P3-NEXT:  ^bb4:
// P3-NEXT:    %22 = "llvm.mul"(%18, %3) : (index, index) -> index
// P3-NEXT:    %23 = "llvm.mul"(%19, %4) : (index, index) -> index
// P3-NEXT:    %24 = "llvm.add"(%22, %23) : (index, index) -> index
// P3-NEXT:    %25 = "llvm.getelementptr"(%14, %24) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P3-NEXT:    %26 = llvm.load %25 : !llvm.ptr -> f32
// P3-NEXT:    %27 = "llvm.fadd"(%20, %26) : (f32, f32) -> f32
// P3-NEXT:    %28 = "llvm.add"(%19, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%18, %28, %27)[^bb3] : (index, index, f32) -> ()
// P3-NEXT:  ^bb5:
// P3-NEXT:    %29 = "llvm.add"(%18, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%29, %20)[^bb0] : (index, f32) -> ()
// P3-NEXT:  ^bb2:
// P3-NEXT:    "llvm.call"(%14) <{callee = @free}> : (!llvm.ptr) -> ()
// P3-NEXT:    "llvm.return"(%16) : (f32) -> ()
// P3-NEXT:  }

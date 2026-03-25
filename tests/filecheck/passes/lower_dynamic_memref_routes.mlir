// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm-pipeline | filecheck %s --check-prefix=P1
// RUN: scair-opt %s -p lower-refined-dmemref-to-llvm-baseline | filecheck %s --check-prefix=P2
// RUN: scair-opt %s -p lower-refined-dmemref-to-llvm-optimized | filecheck %s --check-prefix=P3


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
// P1-NEXT:    %2 = llvm.mlir.constant 256 : index : index
// P1-NEXT:    %3 = llvm.mlir.constant 1024 : index : index
// P1-NEXT:    %4 = llvm.mlir.constant 0 : index : index
// P1-NEXT:    %5 = llvm.mlir.constant 0 : i32 : i32
// P1-NEXT:    %6 = llvm.mlir.constant 0.0 : f32 : f32
// P1-NEXT:    %7 = llvm.mlir.constant 1.0 : f32 : f32
// P1-NEXT:    %8 = "llvm.mul"(%2, %0) : (index, index) -> index
// P1-NEXT:    %9 = llvm.mlir.constant 1 : index : index
// P1-NEXT:    %10 = llvm.mlir.zero : !llvm.ptr
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
// P1-NEXT:    "llvm.cond_br"(%31, %30)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 1, 0>}> : (i1, index) -> ()
// P1-NEXT:  ^bb1(%32: index):
// P1-NEXT:    "llvm.br"(%4, %32)[^bb3] : (index, index) -> ()
// P1-NEXT:  ^bb2:
// P1-NEXT:    "llvm.br"(%4, %6)[^bb4] : (index, f32) -> ()
// P1-NEXT:  ^bb3(%33: index, %34: index):
// P1-NEXT:    %35 = "llvm.icmp"(%33, %3) <{predicate = "slt"}> : (index, index) -> i1
// P1-NEXT:    "llvm.cond_br"(%35, %33, %34, %34)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 2, 1>}> : (i1, index, index, index) -> ()
// P1-NEXT:  ^bb5(%36: index, %37: index):
// P1-NEXT:    "memref.store"(%7, %29, %37, %36) : (f32, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>, index, index) -> ()
// P1-NEXT:    %38 = llvm.mlir.constant 1 : index : index
// P1-NEXT:    %39 = "llvm.add"(%36, %38) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%39, %37)[^bb3] : (index, index) -> ()
// P1-NEXT:  ^bb6(%40: index):
// P1-NEXT:    %41 = llvm.mlir.constant 1 : index : index
// P1-NEXT:    %42 = "llvm.add"(%40, %41) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%42)[^bb0] : (index) -> ()
// P1-NEXT:  ^bb4(%43: index, %44: f32):
// P1-NEXT:    %45 = "llvm.icmp"(%43, %2) <{predicate = "slt"}> : (index, index) -> i1
// P1-NEXT:    "llvm.cond_br"(%45, %43, %44, %44)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 2, 1>}> : (i1, index, f32, f32) -> ()
// P1-NEXT:  ^bb7(%46: index, %47: f32):
// P1-NEXT:    "llvm.br"(%46, %4, %47)[^bb9] : (index, index, f32) -> ()
// P1-NEXT:  ^bb9(%48: index, %49: index, %50: f32):
// P1-NEXT:    %51 = "llvm.icmp"(%49, %3) <{predicate = "slt"}> : (index, index) -> i1
// P1-NEXT:    "llvm.cond_br"(%51, %48, %49, %50, %48, %50)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 3, 2>}> : (i1, index, index, f32, index, f32) -> ()
// P1-NEXT:  ^bb10(%52: index, %53: index, %54: f32):
// P1-NEXT:    %55 = "llvm.extractvalue"(%29) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// P1-NEXT:    %56 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P1-NEXT:    %57 = "llvm.mul"(%52, %56) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %58 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P1-NEXT:    %59 = "llvm.mul"(%53, %58) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %60 = "llvm.add"(%57, %59) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %61 = "llvm.getelementptr"(%55, %60) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// P1-NEXT:    %62 = llvm.load %61 : !llvm.ptr -> f32
// P1-NEXT:    %63 = "llvm.fadd"(%54, %62) : (f32, f32) -> f32
// P1-NEXT:    %64 = llvm.mlir.constant 1 : index : index
// P1-NEXT:    %65 = "llvm.add"(%53, %64) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%52, %65, %63)[^bb9] : (index, index, f32) -> ()
// P1-NEXT:  ^bb11(%66: index, %67: f32):
// P1-NEXT:    %68 = llvm.mlir.constant 1 : index : index
// P1-NEXT:    %69 = "llvm.add"(%66, %68) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%69, %67)[^bb4] : (index, f32) -> ()
// P1-NEXT:  ^bb8(%70: f32):
// P1-NEXT:    %71 = "llvm.extractvalue"(%19) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P1-NEXT:    "llvm.call"(%71) <{callee = @free}> : (!llvm.ptr) -> ()
// P1-NEXT:    "llvm.return"(%70) : (f32) -> ()
// P1-NEXT:  }

// P2-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// P2-NEXT:    %2 = llvm.mlir.constant 256 : index : index
// P2-NEXT:    %3 = llvm.mlir.constant 1024 : index : index
// P2-NEXT:    %4 = llvm.mlir.constant 0 : index : index
// P2-NEXT:    %5 = llvm.mlir.constant 0 : i32 : i32
// P2-NEXT:    %6 = llvm.mlir.constant 0.0 : f32 : f32
// P2-NEXT:    %7 = llvm.mlir.constant 1.0 : f32 : f32
// P2-NEXT:    %8 = "llvm.mul"(%2, %0) : (index, index) -> index
// P2-NEXT:    %9 = llvm.mlir.constant 1 : index : index
// P2-NEXT:    %10 = llvm.mlir.zero : !llvm.ptr
// P2-NEXT:    %11 = "llvm.getelementptr"(%10, %8) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P2-NEXT:    %12 = "llvm.ptrtoint"(%11) : (!llvm.ptr) -> index
// P2-NEXT:    %13 = "llvm.call"(%12) <{callee = @malloc}> : (index) -> !llvm.ptr
// P2-NEXT:    %14 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %15 = "llvm.insertvalue"(%13, %14) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %16 = "llvm.insertvalue"(%13, %15) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %17 = "llvm.insertvalue"(%4, %16) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %18 = "llvm.insertvalue"(%8, %17) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %19 = "llvm.insertvalue"(%9, %18) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %20 = llvm.mlir.constant 256 : index : index
// P2-NEXT:    %21 = llvm.mlir.constant 1024 : index : index
// P2-NEXT:    %22 = "llvm.extractvalue"(%19) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P2-NEXT:    %23 = "llvm.extractvalue"(%19) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P2-NEXT:    %24 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %25 = "llvm.insertvalue"(%22, %24) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %26 = "llvm.insertvalue"(%23, %25) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %27 = "llvm.insertvalue"(%4, %26) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %28 = "llvm.insertvalue"(%20, %27) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %29 = "llvm.insertvalue"(%21, %28) <{position = array<i32: 3, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %30 = "llvm.insertvalue"(%0, %29) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %31 = "llvm.insertvalue"(%1, %30) <{position = array<i32: 4, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    "llvm.br"(%4)[^bb0] : (index) -> ()
// P2-NEXT:  ^bb0(%32: index):
// P2-NEXT:    %33 = "llvm.icmp"(%32, %2) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%33, %32)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 1, 0>}> : (i1, index) -> ()
// P2-NEXT:  ^bb1(%34: index):
// P2-NEXT:    "llvm.br"(%4, %34)[^bb3] : (index, index) -> ()
// P2-NEXT:  ^bb2:
// P2-NEXT:    "llvm.br"(%4, %6)[^bb4] : (index, f32) -> ()
// P2-NEXT:  ^bb3(%35: index, %36: index):
// P2-NEXT:    %37 = "llvm.icmp"(%35, %3) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%37, %35, %36, %36)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 2, 1>}> : (i1, index, index, index) -> ()
// P2-NEXT:  ^bb5(%38: index, %39: index):
// P2-NEXT:    "memref.store"(%7, %31, %39, %38) : (f32, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>, index, index) -> ()
// P2-NEXT:    %40 = "llvm.add"(%38, %9) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%40, %39)[^bb3] : (index, index) -> ()
// P2-NEXT:  ^bb6(%41: index):
// P2-NEXT:    %42 = "llvm.add"(%41, %9) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%42)[^bb0] : (index) -> ()
// P2-NEXT:  ^bb4(%43: index, %44: f32):
// P2-NEXT:    %45 = "llvm.icmp"(%43, %2) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%45, %43, %44, %44)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 2, 1>}> : (i1, index, f32, f32) -> ()
// P2-NEXT:  ^bb7(%46: index, %47: f32):
// P2-NEXT:    "llvm.br"(%46, %4, %47)[^bb9] : (index, index, f32) -> ()
// P2-NEXT:  ^bb9(%48: index, %49: index, %50: f32):
// P2-NEXT:    %51 = "llvm.icmp"(%49, %3) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%51, %48, %49, %50, %48, %50)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 3, 2>}> : (i1, index, index, f32, index, f32) -> ()
// P2-NEXT:  ^bb10(%52: index, %53: index, %54: f32):
// P2-NEXT:    %55 = "llvm.extractvalue"(%31) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// P2-NEXT:    %56 = "llvm.extractvalue"(%31) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P2-NEXT:    %57 = "llvm.mul"(%52, %56) : (index, index) -> index
// P2-NEXT:    %58 = "llvm.extractvalue"(%31) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P2-NEXT:    %59 = "llvm.mul"(%53, %58) : (index, index) -> index
// P2-NEXT:    %60 = "llvm.add"(%57, %59) : (index, index) -> index
// P2-NEXT:    %61 = "llvm.getelementptr"(%55, %60) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P2-NEXT:    %62 = llvm.load %61 : !llvm.ptr -> f32
// P2-NEXT:    %63 = "llvm.fadd"(%54, %62) : (f32, f32) -> f32
// P2-NEXT:    %64 = "llvm.add"(%53, %9) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%52, %64, %63)[^bb9] : (index, index, f32) -> ()
// P2-NEXT:  ^bb11(%65: index, %66: f32):
// P2-NEXT:    %67 = "llvm.add"(%65, %9) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%67, %66)[^bb4] : (index, f32) -> ()
// P2-NEXT:  ^bb8(%68: f32):
// P2-NEXT:    %69 = "llvm.extractvalue"(%19) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P2-NEXT:    "llvm.call"(%69) <{callee = @free}> : (!llvm.ptr) -> ()
// P2-NEXT:    "llvm.return"(%68) : (f32) -> ()
// P2-NEXT:  }

// P3-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// P3-NEXT:    %2 = llvm.mlir.constant 0 : index : index
// P3-NEXT:    %3 = "llvm.add"(%0, %2) : (index, index) -> index
// P3-NEXT:    %4 = "llvm.add"(%1, %2) : (index, index) -> index
// P3-NEXT:    %5 = llvm.mlir.constant 1 : index : index
// P3-NEXT:    %6 = llvm.mlir.constant 256 : index : index
// P3-NEXT:    %7 = llvm.mlir.constant 1024 : index : index
// P3-NEXT:    %8 = llvm.mlir.constant 0 : index : index
// P3-NEXT:    %9 = llvm.mlir.constant 0.0 : f32 : f32
// P3-NEXT:    %10 = llvm.mlir.constant 1.0 : f32 : f32
// P3-NEXT:    %11 = "llvm.mul"(%6, %3) : (index, index) -> index
// P3-NEXT:    %12 = llvm.mlir.zero : !llvm.ptr
// P3-NEXT:    %13 = "llvm.getelementptr"(%12, %11) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P3-NEXT:    %14 = "llvm.ptrtoint"(%13) : (!llvm.ptr) -> index
// P3-NEXT:    %15 = "llvm.call"(%14) <{callee = @malloc}> : (index) -> !llvm.ptr
// P3-NEXT:    "llvm.br"(%8)[^bb0] : (index) -> ()
// P3-NEXT:  ^bb0(%16: index):
// P3-NEXT:    %17 = "llvm.icmp"(%16, %6) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%17, %16)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 1, 0>}> : (i1, index) -> ()
// P3-NEXT:  ^bb1(%18: index):
// P3-NEXT:    "llvm.br"(%8, %18)[^bb3] : (index, index) -> ()
// P3-NEXT:  ^bb2:
// P3-NEXT:    "llvm.br"(%8, %9)[^bb4] : (index, f32) -> ()
// P3-NEXT:  ^bb3(%19: index, %20: index):
// P3-NEXT:    %21 = "llvm.icmp"(%19, %7) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%21, %19, %20, %20)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 2, 1>}> : (i1, index, index, index) -> ()
// P3-NEXT:  ^bb5(%22: index, %23: index):
// P3-NEXT:    "memref.store"(%10, %15, %23, %22) : (f32, !llvm.ptr, index, index) -> ()
// P3-NEXT:    %24 = "llvm.add"(%22, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%24, %23)[^bb3] : (index, index) -> ()
// P3-NEXT:  ^bb6(%25: index):
// P3-NEXT:    %26 = "llvm.add"(%25, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%26)[^bb0] : (index) -> ()
// P3-NEXT:  ^bb4(%27: index, %28: f32):
// P3-NEXT:    %29 = "llvm.icmp"(%27, %6) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%29, %27, %28, %28)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 2, 1>}> : (i1, index, f32, f32) -> ()
// P3-NEXT:  ^bb7(%30: index, %31: f32):
// P3-NEXT:    %32 = "llvm.mul"(%30, %3) : (index, index) -> index
// P3-NEXT:    "llvm.br"(%30, %8, %31, %32)[^bb9] : (index, index, f32, index) -> ()
// P3-NEXT:  ^bb9(%33: index, %34: index, %35: f32, %36: index):
// P3-NEXT:    %37 = "llvm.icmp"(%34, %7) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%37, %33, %34, %35, %36, %33, %35)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 4, 2>}> : (i1, index, index, f32, index, index, f32) -> ()
// P3-NEXT:  ^bb10(%38: index, %39: index, %40: f32, %41: index):
// P3-NEXT:    %42 = "llvm.mul"(%39, %4) : (index, index) -> index
// P3-NEXT:    %43 = "llvm.add"(%8, %41) : (index, index) -> index
// P3-NEXT:    %44 = "llvm.add"(%43, %42) : (index, index) -> index
// P3-NEXT:    %45 = "llvm.getelementptr"(%15, %44) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P3-NEXT:    %46 = llvm.load %45 : !llvm.ptr -> f32
// P3-NEXT:    %47 = "llvm.fadd"(%40, %46) : (f32, f32) -> f32
// P3-NEXT:    %48 = "llvm.add"(%39, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%38, %48, %47, %41)[^bb9] : (index, index, f32, index) -> ()
// P3-NEXT:  ^bb11(%49: index, %50: f32):
// P3-NEXT:    %51 = "llvm.add"(%49, %5) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%51, %50)[^bb4] : (index, f32) -> ()
// P3-NEXT:  ^bb8(%52: f32):
// P3-NEXT:    "llvm.call"(%15) <{callee = @free}> : (!llvm.ptr) -> ()
// P3-NEXT:    "llvm.return"(%52) : (f32) -> ()
// P3-NEXT:  }

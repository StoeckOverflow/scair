// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm-baseline | filecheck %s --check-prefix=P1
// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm | filecheck %s --check-prefix=P2
// RUN: scair-opt %S/lower_refined_dmemref_to_llvm.mlir -p lower-dmemref-to-llvm | filecheck %s --check-prefix=P3


// builtin.module {
//   func.func @semi_affine_layout_map(%stride0 : i64, %stride1 : i64) -> f32 {
//     %c256 = "arith.constant"() <{value = 256}> : () -> i64
//     %total = "arith.muli"(%c256, %stride0) : (i64, i64) -> i64
//     %flat = "memref.alloc"(%total) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}> : (i64) -> memref<?xf32>
//     %c1024 = "arith.constant"() <{value = 1024}> : () -> i64
//     %c0 = "arith.constant"() <{value = 0}> : () -> i64
//     %buf = "memref.reinterpret_cast"(%flat, %c0, %c256, %c1024, %stride0, %stride1) <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}> : (memref<?xf32>, i64, i64, i64, i64, i64) -> memref<256x1024xf32, strided<[?, ?], offset: 0>>
//     %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
//     %result = "affine.for"(%c0, %c256, %cst) <{lowerBoundMap = affine_map<(d0) -> (d0)>, upperBoundMap = affine_map<(d0) -> (d0)>, step = 1 : i64, operandSegmentSizes = array<i32: 1, 1, 1>}> ({
//     ^bb0(%i: i64, %acc: f32):
//       %inner = "affine.for"(%c0, %c1024, %acc) <{lowerBoundMap = affine_map<(d0) -> (d0)>, upperBoundMap = affine_map<(d0) -> (d0)>, step = 1 : i64, operandSegmentSizes = array<i32: 1, 1, 1>}> ({
//       ^bb0(%j: i64, %acc2: f32):
//         %v = "memref.load"(%buf, %i, %j) : (memref<256x1024xf32, strided<[?, ?], offset: 0>>, i64, i64) -> f32
//         %sum = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
//         "affine.yield"(%sum) : (f32) -> ()
//       }) : (i64, i64, f32) -> f32
//       "affine.yield"(%inner) : (f32) -> ()
//     }) : (i64, i64, f32) -> f32
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

// P1-LABEL: func.func @semi_affine_layout_map(%0: i64, %1: i64) -> f32 {
// P1-NEXT:    %2 = "llvm.mlir.constant"() <{value = 256}> : () -> i64
// P1-NEXT:    %3 = "llvm.mlir.constant"() <{value = 1024}> : () -> i64
// P1-NEXT:    %4 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P1-NEXT:    %5 = "llvm.mlir.constant"() <{value = 0 : i32}> : () -> i32
// P1-NEXT:    %6 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P1-NEXT:    %7 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// P1-NEXT:    %8 = "llvm.mul"(%2, %0) : (i64, i64) -> i64
// P1-NEXT:    %9 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P1-NEXT:    %10 = "llvm.mlir.zero"() : () -> !llvm.ptr
// P1-NEXT:    %11 = "llvm.getelementptr"(%10, %8) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P1-NEXT:    %12 = "llvm.ptrtoint"(%11) : (!llvm.ptr) -> i64
// P1-NEXT:    %13 = llvm.call @malloc(%12) : (i64) -> !llvm.ptr
// P1-NEXT:    %14 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %15 = llvm.insertvalue %13, %14[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %16 = llvm.insertvalue %13, %15[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %17 = llvm.insertvalue %5, %16[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %18 = llvm.insertvalue %8, %17[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %19 = llvm.insertvalue %9, %18[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %20 = llvm.extractvalue %19[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %21 = llvm.extractvalue %19[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %22 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %23 = llvm.insertvalue %20, %22[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %24 = llvm.insertvalue %21, %23[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %25 = llvm.insertvalue %4, %24[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %26 = llvm.insertvalue %2, %25[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %27 = llvm.insertvalue %3, %26[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %28 = llvm.insertvalue %0, %27[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %29 = llvm.insertvalue %1, %28[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    "llvm.br"(%4)[^bb0] : (i64) -> ()
// P1-NEXT:  ^bb0(%30: i64):
// P1-NEXT:    %31 = llvm.icmp "slt" %30, %2 : i64
// P1-NEXT:    "llvm.cond_br"(%31)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P1-NEXT:  ^bb1:
// P1-NEXT:    "llvm.br"(%4)[^bb3] : (i64) -> ()
// P1-NEXT:  ^bb2:
// P1-NEXT:    "llvm.br"(%4, %6)[^bb4] : (i64, f32) -> ()
// P1-NEXT:  ^bb3(%32: i64):
// P1-NEXT:    %33 = llvm.icmp "slt" %32, %3 : i64
// P1-NEXT:    "llvm.cond_br"(%33)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P1-NEXT:  ^bb5:
// P1-NEXT:    %34 = llvm.extractvalue %29[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %35 = llvm.extractvalue %29[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %36 = "llvm.mul"(%30, %35) : (i64, i64) -> i64
// P1-NEXT:    %37 = llvm.extractvalue %29[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %38 = "llvm.mul"(%32, %37) : (i64, i64) -> i64
// P1-NEXT:    %39 = "llvm.add"(%36, %38) : (i64, i64) -> i64
// P1-NEXT:    %40 = "llvm.getelementptr"(%34, %39) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P1-NEXT:    "llvm.store"(%7, %40) : (f32, !llvm.ptr) -> ()
// P1-NEXT:    %41 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P1-NEXT:    %42 = "llvm.add"(%32, %41) : (i64, i64) -> i64
// P1-NEXT:    "llvm.br"(%42)[^bb3] : (i64) -> ()
// P1-NEXT:  ^bb6:
// P1-NEXT:    %43 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P1-NEXT:    %44 = "llvm.add"(%30, %43) : (i64, i64) -> i64
// P1-NEXT:    "llvm.br"(%44)[^bb0] : (i64) -> ()
// P1-NEXT:  ^bb4(%45: i64, %46: f32):
// P1-NEXT:    %47 = llvm.icmp "slt" %45, %2 : i64
// P1-NEXT:    "llvm.cond_br"(%47)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P1-NEXT:  ^bb7:
// P1-NEXT:    "llvm.br"(%4, %46)[^bb9] : (i64, f32) -> ()
// P1-NEXT:  ^bb9(%48: i64, %49: f32):
// P1-NEXT:    %50 = llvm.icmp "slt" %48, %3 : i64
// P1-NEXT:    "llvm.cond_br"(%50)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P1-NEXT:  ^bb10:
// P1-NEXT:    %51 = llvm.extractvalue %29[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %52 = llvm.extractvalue %29[4, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %53 = "llvm.mul"(%45, %52) : (i64, i64) -> i64
// P1-NEXT:    %54 = llvm.extractvalue %29[4, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %55 = "llvm.mul"(%48, %54) : (i64, i64) -> i64
// P1-NEXT:    %56 = "llvm.add"(%53, %55) : (i64, i64) -> i64
// P1-NEXT:    %57 = "llvm.getelementptr"(%51, %56) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P1-NEXT:    %58 = llvm.load %57 : !llvm.ptr -> f32
// P1-NEXT:    %59 = "llvm.fadd"(%49, %58) : (f32, f32) -> f32
// P1-NEXT:    %60 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P1-NEXT:    %61 = "llvm.add"(%48, %60) : (i64, i64) -> i64
// P1-NEXT:    "llvm.br"(%61, %59)[^bb9] : (i64, f32) -> ()
// P1-NEXT:  ^bb11:
// P1-NEXT:    %62 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P1-NEXT:    %63 = "llvm.add"(%45, %62) : (i64, i64) -> i64
// P1-NEXT:    "llvm.br"(%63, %49)[^bb4] : (i64, f32) -> ()
// P1-NEXT:  ^bb8:
// P1-NEXT:    %64 = llvm.extractvalue %19[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    llvm.call @free(%64) : (!llvm.ptr) -> ()
// P1-NEXT:    "llvm.return"(%46) : (f32) -> ()
// P1-NEXT:  }

// P2-LABEL: func.func @semi_affine_layout_map(%0: i64, %1: i64) -> f32 {
// P2-NEXT:    %2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P2-NEXT:    %3 = "llvm.add"(%0, %2) : (i64, i64) -> i64
// P2-NEXT:    %4 = "llvm.add"(%1, %2) : (i64, i64) -> i64
// P2-NEXT:    %5 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P2-NEXT:    %6 = "llvm.mlir.constant"() <{value = 256}> : () -> i64
// P2-NEXT:    %7 = "llvm.mlir.constant"() <{value = 1024}> : () -> i64
// P2-NEXT:    %8 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P2-NEXT:    %9 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P2-NEXT:    %10 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// P2-NEXT:    %11 = "llvm.mul"(%6, %3) : (i64, i64) -> i64
// P2-NEXT:    %12 = "llvm.mlir.zero"() : () -> !llvm.ptr
// P2-NEXT:    %13 = "llvm.getelementptr"(%12, %11) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P2-NEXT:    %14 = "llvm.ptrtoint"(%13) : (!llvm.ptr) -> i64
// P2-NEXT:    %15 = llvm.call @malloc(%14) : (i64) -> !llvm.ptr
// P2-NEXT:    "llvm.br"(%8)[^bb0] : (i64) -> ()
// P2-NEXT:  ^bb0(%16: i64):
// P2-NEXT:    %17 = llvm.icmp "slt" %16, %6 : i64
// P2-NEXT:    "llvm.cond_br"(%17)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P2-NEXT:  ^bb1:
// P2-NEXT:    "llvm.br"(%8)[^bb3] : (i64) -> ()
// P2-NEXT:  ^bb2:
// P2-NEXT:    "llvm.br"(%8, %9)[^bb4] : (i64, f32) -> ()
// P2-NEXT:  ^bb3(%18: i64):
// P2-NEXT:    %19 = llvm.icmp "slt" %18, %7 : i64
// P2-NEXT:    "llvm.cond_br"(%19)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P2-NEXT:  ^bb5:
// P2-NEXT:    %20 = "llvm.mul"(%16, %3) : (i64, i64) -> i64
// P2-NEXT:    %21 = "llvm.mul"(%18, %4) : (i64, i64) -> i64
// P2-NEXT:    %22 = "llvm.add"(%8, %20) : (i64, i64) -> i64
// P2-NEXT:    %23 = "llvm.add"(%22, %21) : (i64, i64) -> i64
// P2-NEXT:    %24 = "llvm.getelementptr"(%15, %23) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P2-NEXT:    "llvm.store"(%10, %24) : (f32, !llvm.ptr) -> ()
// P2-NEXT:    %25 = "llvm.add"(%18, %5) : (i64, i64) -> i64
// P2-NEXT:    "llvm.br"(%25)[^bb3] : (i64) -> ()
// P2-NEXT:  ^bb6:
// P2-NEXT:    %26 = "llvm.add"(%16, %5) : (i64, i64) -> i64
// P2-NEXT:    "llvm.br"(%26)[^bb0] : (i64) -> ()
// P2-NEXT:  ^bb4(%27: i64, %28: f32):
// P2-NEXT:    %29 = llvm.icmp "slt" %27, %6 : i64
// P2-NEXT:    "llvm.cond_br"(%29)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P2-NEXT:  ^bb7:
// P2-NEXT:    "llvm.br"(%27, %8, %28)[^bb9] : (i64, i64, f32) -> ()
// P2-NEXT:  ^bb9(%30: i64, %31: i64, %32: f32):
// P2-NEXT:    %33 = llvm.icmp "slt" %31, %7 : i64
// P2-NEXT:    "llvm.cond_br"(%33)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P2-NEXT:  ^bb10:
// P2-NEXT:    %34 = "llvm.mul"(%30, %3) : (i64, i64) -> i64
// P2-NEXT:    %35 = "llvm.mul"(%31, %4) : (i64, i64) -> i64
// P2-NEXT:    %36 = "llvm.add"(%8, %34) : (i64, i64) -> i64
// P2-NEXT:    %37 = "llvm.add"(%36, %35) : (i64, i64) -> i64
// P2-NEXT:    %38 = "llvm.getelementptr"(%15, %37) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P2-NEXT:    %39 = llvm.load %38 : !llvm.ptr -> f32
// P2-NEXT:    %40 = "llvm.fadd"(%32, %39) : (f32, f32) -> f32
// P2-NEXT:    %41 = "llvm.add"(%31, %5) : (i64, i64) -> i64
// P2-NEXT:    "llvm.br"(%30, %41, %40)[^bb9] : (i64, i64, f32) -> ()
// P2-NEXT:  ^bb11:
// P2-NEXT:    %42 = "llvm.add"(%30, %5) : (i64, i64) -> i64
// P2-NEXT:    "llvm.br"(%42, %32)[^bb4] : (i64, f32) -> ()
// P2-NEXT:  ^bb8:
// P2-NEXT:    llvm.call @free(%15) : (!llvm.ptr) -> ()
// P2-NEXT:    "llvm.return"(%28) : (f32) -> ()
// P2-NEXT:  }

// P3-LABEL: func.func @semi_affine_layout_map(%0: i64, %1: i64) -> f32 {
// P3-NEXT:    %2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P3-NEXT:    %3 = "llvm.add"(%0, %2) : (i64, i64) -> i64
// P3-NEXT:    %4 = "llvm.add"(%1, %2) : (i64, i64) -> i64
// P3-NEXT:    %5 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P3-NEXT:    %6 = "llvm.mlir.constant"() <{value = 256}> : () -> i64
// P3-NEXT:    %7 = "llvm.mlir.constant"() <{value = 1024}> : () -> i64
// P3-NEXT:    %8 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P3-NEXT:    %9 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P3-NEXT:    %10 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// P3-NEXT:    %11 = "llvm.mul"(%6, %3) : (i64, i64) -> i64
// P3-NEXT:    %12 = "llvm.mlir.zero"() : () -> !llvm.ptr
// P3-NEXT:    %13 = "llvm.getelementptr"(%12, %11) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P3-NEXT:    %14 = "llvm.ptrtoint"(%13) : (!llvm.ptr) -> i64
// P3-NEXT:    %15 = llvm.call @malloc(%14) : (i64) -> !llvm.ptr
// P3-NEXT:    "llvm.br"(%8)[^bb0] : (i64) -> ()
// P3-NEXT:  ^bb0(%16: i64):
// P3-NEXT:    %17 = llvm.icmp "slt" %16, %6 : i64
// P3-NEXT:    "llvm.cond_br"(%17)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P3-NEXT:  ^bb1:
// P3-NEXT:    "llvm.br"(%8)[^bb3] : (i64) -> ()
// P3-NEXT:  ^bb2:
// P3-NEXT:    "llvm.br"(%8, %9)[^bb4] : (i64, f32) -> ()
// P3-NEXT:  ^bb3(%18: i64):
// P3-NEXT:    %19 = llvm.icmp "slt" %18, %7 : i64
// P3-NEXT:    "llvm.cond_br"(%19)[^bb5, ^bb6] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P3-NEXT:  ^bb5:
// P3-NEXT:    %20 = "llvm.mul"(%16, %3) : (i64, i64) -> i64
// P3-NEXT:    %21 = "llvm.mul"(%18, %4) : (i64, i64) -> i64
// P3-NEXT:    %22 = "llvm.add"(%8, %20) : (i64, i64) -> i64
// P3-NEXT:    %23 = "llvm.add"(%22, %21) : (i64, i64) -> i64
// P3-NEXT:    %24 = "llvm.getelementptr"(%15, %23) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P3-NEXT:    "llvm.store"(%10, %24) : (f32, !llvm.ptr) -> ()
// P3-NEXT:    %25 = "llvm.add"(%18, %5) : (i64, i64) -> i64
// P3-NEXT:    "llvm.br"(%25)[^bb3] : (i64) -> ()
// P3-NEXT:  ^bb6:
// P3-NEXT:    %26 = "llvm.add"(%16, %5) : (i64, i64) -> i64
// P3-NEXT:    "llvm.br"(%26)[^bb0] : (i64) -> ()
// P3-NEXT:  ^bb4(%27: i64, %28: f32):
// P3-NEXT:    %29 = llvm.icmp "slt" %27, %6 : i64
// P3-NEXT:    "llvm.cond_br"(%29)[^bb7, ^bb8] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P3-NEXT:  ^bb7:
// P3-NEXT:    "llvm.br"(%27, %8, %28)[^bb9] : (i64, i64, f32) -> ()
// P3-NEXT:  ^bb9(%30: i64, %31: i64, %32: f32):
// P3-NEXT:    %33 = llvm.icmp "slt" %31, %7 : i64
// P3-NEXT:    "llvm.cond_br"(%33)[^bb10, ^bb11] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P3-NEXT:  ^bb10:
// P3-NEXT:    %34 = "llvm.mul"(%30, %3) : (i64, i64) -> i64
// P3-NEXT:    %35 = "llvm.mul"(%31, %4) : (i64, i64) -> i64
// P3-NEXT:    %36 = "llvm.add"(%8, %34) : (i64, i64) -> i64
// P3-NEXT:    %37 = "llvm.add"(%36, %35) : (i64, i64) -> i64
// P3-NEXT:    %38 = "llvm.getelementptr"(%15, %37) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P3-NEXT:    %39 = llvm.load %38 : !llvm.ptr -> f32
// P3-NEXT:    %40 = "llvm.fadd"(%32, %39) : (f32, f32) -> f32
// P3-NEXT:    %41 = "llvm.add"(%31, %5) : (i64, i64) -> i64
// P3-NEXT:    "llvm.br"(%30, %41, %40)[^bb9] : (i64, i64, f32) -> ()
// P3-NEXT:  ^bb11:
// P3-NEXT:    %42 = "llvm.add"(%30, %5) : (i64, i64) -> i64
// P3-NEXT:    "llvm.br"(%42, %32)[^bb4] : (i64, f32) -> ()
// P3-NEXT:  ^bb8:
// P3-NEXT:    llvm.call @free(%15) : (!llvm.ptr) -> ()
// P3-NEXT:    "llvm.return"(%28) : (f32) -> ()
// P3-NEXT:  }

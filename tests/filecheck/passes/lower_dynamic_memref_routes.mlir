// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm-pipeline | filecheck %s --check-prefix=P1
// RUN: scair-opt %s -p lower-refined-dmemref-to-llvm-baseline | filecheck %s --check-prefix=P2
// RUN: scair-opt %s -p lower-refined-dmemref-to-llvm-optimized | filecheck %s --check-prefix=P3

builtin.module {
  func.func @semi_affine_layout_map(%stride0 : index, %stride1 : index) -> f32 {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %total = "arith.muli"(%c256, %stride0) : (index, index) -> index
    %flat = "memref.alloc"(%total) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}> : (index) -> memref<?xf32>
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %buf = "memref.reinterpret_cast"(%flat, %c0, %c256, %c1024, %stride0, %stride1) <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}> : (memref<?xf32>, index, index, index, index, index) -> memref<256x1024xf32, strided<[?, ?], offset: 0>>
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %result = "affine.for"(%c0, %c256, %cst) <{lowerBoundMap = affine_map<(d0) -> (d0)>, upperBoundMap = affine_map<(d0) -> (d0)>, step = 1 : index, operandSegmentSizes = array<i32: 1, 1, 1>}> ({
    ^bb0(%i: index, %acc: f32):
      %inner = "affine.for"(%c0, %c1024, %acc) <{lowerBoundMap = affine_map<(d0) -> (d0)>, upperBoundMap = affine_map<(d0) -> (d0)>, step = 1 : index, operandSegmentSizes = array<i32: 1, 1, 1>}> ({
      ^bb0(%j: index, %acc2: f32):
        %v = "memref.load"(%buf, %i, %j) : (memref<256x1024xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
        %sum = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        "affine.yield"(%sum) : (f32) -> ()
      }) : (index, index, f32) -> f32
      "affine.yield"(%inner) : (f32) -> ()
    }) : (index, index, f32) -> f32
    memref.dealloc %flat : memref<?xf32>
    func.return %result : f32
  }
}

// P1-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// P1-NEXT:    %2 = llvm.mlir.constant 256 : index : index
// P1-NEXT:    %3 = llvm.mlir.constant 1024 : index : index
// P1-NEXT:    %4 = llvm.mlir.constant 0 : index : index
// P1-NEXT:    %5 = llvm.mlir.constant 0.0 : f32 : f32
// P1-NEXT:    %6 = "llvm.mul"(%2, %0) : (index, index) -> index
// P1-NEXT:    %7 = llvm.mlir.constant 1 : index : index
// P1-NEXT:    %8 = llvm.mlir.zero : !llvm.ptr
// P1-NEXT:    %9 = "llvm.getelementptr"(%8, %6) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P1-NEXT:    %10 = "llvm.ptrtoint"(%9) : (!llvm.ptr) -> index
// P1-NEXT:    %11 = "llvm.call"(%10) <{callee = @malloc}> : (index) -> !llvm.ptr
// P1-NEXT:    %12 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %13 = "llvm.insertvalue"(%11, %12) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %14 = "llvm.insertvalue"(%11, %13) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %15 = "llvm.insertvalue"(%4, %14) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %16 = "llvm.insertvalue"(%6, %15) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %17 = "llvm.insertvalue"(%7, %16) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P1-NEXT:    %18 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P1-NEXT:    %19 = "llvm.extractvalue"(%17) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P1-NEXT:    %20 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %21 = "llvm.insertvalue"(%18, %20) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %22 = "llvm.insertvalue"(%19, %21) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %23 = "llvm.insertvalue"(%4, %22) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %24 = "llvm.insertvalue"(%2, %23) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %25 = "llvm.insertvalue"(%3, %24) <{position = array<i32: 3, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %26 = "llvm.insertvalue"(%0, %25) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    %27 = "llvm.insertvalue"(%1, %26) <{position = array<i32: 4, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P1-NEXT:    "llvm.br"(%4, %5)[^bb0] : (index, f32) -> ()
// P1-NEXT:  ^bb0(%28: index, %29: f32):
// P1-NEXT:    %30 = "llvm.icmp"(%28, %2) <{predicate = "slt"}> : (index, index) -> i1
// P1-NEXT:    "llvm.cond_br"(%30, %28, %29, %29)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 2, 1>}> : (i1, index, f32, f32) -> ()
// P1-NEXT:  ^bb1(%31: index, %32: f32):
// P1-NEXT:    "llvm.br"(%31, %4, %32)[^bb3] : (index, index, f32) -> ()
// P1-NEXT:  ^bb3(%33: index, %34: index, %35: f32):
// P1-NEXT:    %36 = "llvm.icmp"(%34, %3) <{predicate = "slt"}> : (index, index) -> i1
// P1-NEXT:    "llvm.cond_br"(%36, %33, %34, %35, %33, %35)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 3, 2>}> : (i1, index, index, f32, index, f32) -> ()
// P1-NEXT:  ^bb4(%37: index, %38: index, %39: f32):
// P1-NEXT:    %40 = "llvm.extractvalue"(%27) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// P1-NEXT:    %41 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P1-NEXT:    %42 = "llvm.mul"(%37, %41) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %43 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P1-NEXT:    %44 = "llvm.mul"(%38, %43) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %45 = "llvm.add"(%42, %44) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    %46 = "llvm.getelementptr"(%40, %45) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// P1-NEXT:    %47 = llvm.load %46 : !llvm.ptr -> f32
// P1-NEXT:    %48 = "llvm.fadd"(%39, %47) : (f32, f32) -> f32
// P1-NEXT:    %49 = llvm.mlir.constant 1 : index : index
// P1-NEXT:    %50 = "llvm.add"(%38, %49) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%37, %50, %48)[^bb3] : (index, index, f32) -> ()
// P1-NEXT:  ^bb5(%51: index, %52: f32):
// P1-NEXT:    %53 = llvm.mlir.constant 1 : index : index
// P1-NEXT:    %54 = "llvm.add"(%51, %53) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P1-NEXT:    "llvm.br"(%54, %52)[^bb0] : (index, f32) -> ()
// P1-NEXT:  ^bb2(%55: f32):
// P1-NEXT:    %56 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P1-NEXT:    "llvm.call"(%56) <{callee = @free}> : (!llvm.ptr) -> ()
// P1-NEXT:    "llvm.return"(%55) : (f32) -> ()
// P1-NEXT:  }

// P2-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// P2-NEXT:    %2 = llvm.mlir.constant 256 : index : index
// P2-NEXT:    %3 = llvm.mlir.constant 1024 : index : index
// P2-NEXT:    %4 = llvm.mlir.constant 0 : index : index
// P2-NEXT:    %5 = llvm.mlir.constant 0.0 : f32 : f32
// P2-NEXT:    %6 = "llvm.mul"(%2, %0) : (index, index) -> index
// P2-NEXT:    %7 = llvm.mlir.constant 1 : index : index
// P2-NEXT:    %8 = llvm.mlir.zero : !llvm.ptr
// P2-NEXT:    %9 = "llvm.getelementptr"(%8, %6) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P2-NEXT:    %10 = "llvm.ptrtoint"(%9) : (!llvm.ptr) -> index
// P2-NEXT:    %11 = "llvm.call"(%10) <{callee = @malloc}> : (index) -> !llvm.ptr
// P2-NEXT:    %12 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %13 = "llvm.insertvalue"(%11, %12) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %14 = "llvm.insertvalue"(%11, %13) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %15 = "llvm.insertvalue"(%4, %14) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %16 = "llvm.insertvalue"(%6, %15) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %17 = "llvm.insertvalue"(%7, %16) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P2-NEXT:    %18 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P2-NEXT:    %19 = "llvm.extractvalue"(%17) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P2-NEXT:    %20 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %21 = "llvm.insertvalue"(%18, %20) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %22 = "llvm.insertvalue"(%19, %21) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %23 = "llvm.insertvalue"(%4, %22) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %24 = "llvm.insertvalue"(%2, %23) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %25 = "llvm.insertvalue"(%3, %24) <{position = array<i32: 3, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %26 = "llvm.insertvalue"(%0, %25) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %27 = "llvm.insertvalue"(%1, %26) <{position = array<i32: 4, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    "llvm.br"(%4, %5)[^bb0] : (index, f32) -> ()
// P2-NEXT:  ^bb0(%28: index, %29: f32):
// P2-NEXT:    %30 = "llvm.icmp"(%28, %2) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%30, %28, %29, %29)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 2, 1>}> : (i1, index, f32, f32) -> ()
// P2-NEXT:  ^bb1(%31: index, %32: f32):
// P2-NEXT:    "llvm.br"(%31, %4, %32)[^bb3] : (index, index, f32) -> ()
// P2-NEXT:  ^bb3(%33: index, %34: index, %35: f32):
// P2-NEXT:    %36 = "llvm.icmp"(%34, %3) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%36, %33, %34, %35, %33, %35)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 3, 2>}> : (i1, index, index, f32, index, f32) -> ()
// P2-NEXT:  ^bb4(%37: index, %38: index, %39: f32):
// P2-NEXT:    %40 = "llvm.extractvalue"(%27) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// P2-NEXT:    %41 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P2-NEXT:    %42 = "llvm.mul"(%37, %41) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    %43 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P2-NEXT:    %44 = "llvm.mul"(%38, %43) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    %45 = "llvm.add"(%42, %44) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    %46 = "llvm.getelementptr"(%40, %45) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// P2-NEXT:    %47 = llvm.load %46 : !llvm.ptr -> f32
// P2-NEXT:    %48 = "llvm.fadd"(%39, %47) : (f32, f32) -> f32
// P2-NEXT:    %49 = "llvm.add"(%38, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%37, %49, %48)[^bb3] : (index, index, f32) -> ()
// P2-NEXT:  ^bb5(%50: index, %51: f32):
// P2-NEXT:    %52 = "llvm.add"(%50, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%52, %51)[^bb0] : (index, f32) -> ()
// P2-NEXT:  ^bb2(%53: f32):
// P2-NEXT:    %54 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P2-NEXT:    "llvm.call"(%54) <{callee = @free}> : (!llvm.ptr) -> ()
// P2-NEXT:    "llvm.return"(%53) : (f32) -> ()
// P2-NEXT:  }

// P3-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// P3-NEXT:    %2 = llvm.mlir.constant 256 : index : index
// P3-NEXT:    %3 = llvm.mlir.constant 1024 : index : index
// P3-NEXT:    %4 = llvm.mlir.constant 0 : index : index
// P3-NEXT:    %5 = llvm.mlir.constant 0.0 : f32 : f32
// P3-NEXT:    %6 = "llvm.mul"(%2, %0) : (index, index) -> index
// P3-NEXT:    %7 = llvm.mlir.constant 1 : index : index
// P3-NEXT:    %8 = llvm.mlir.zero : !llvm.ptr
// P3-NEXT:    %9 = "llvm.getelementptr"(%8, %6) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P3-NEXT:    %10 = "llvm.ptrtoint"(%9) : (!llvm.ptr) -> index
// P3-NEXT:    %11 = "llvm.call"(%10) <{callee = @malloc}> : (index) -> !llvm.ptr
// P3-NEXT:    %12 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P3-NEXT:    %13 = "llvm.insertvalue"(%11, %12) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P3-NEXT:    %14 = "llvm.insertvalue"(%11, %13) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P3-NEXT:    %15 = "llvm.insertvalue"(%4, %14) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P3-NEXT:    %16 = "llvm.insertvalue"(%6, %15) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P3-NEXT:    %17 = "llvm.insertvalue"(%7, %16) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// P3-NEXT:    %18 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P3-NEXT:    %19 = "llvm.extractvalue"(%17) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P3-NEXT:    %20 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %21 = "llvm.insertvalue"(%18, %20) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %22 = "llvm.insertvalue"(%19, %21) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %23 = "llvm.insertvalue"(%4, %22) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %24 = "llvm.insertvalue"(%2, %23) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %25 = "llvm.insertvalue"(%3, %24) <{position = array<i32: 3, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %26 = "llvm.insertvalue"(%0, %25) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %27 = "llvm.insertvalue"(%1, %26) <{position = array<i32: 4, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    "llvm.br"(%4, %5, %0, %1)[^bb0] : (index, f32, index, index) -> ()
// P3-NEXT:  ^bb0(%28: index, %29: f32, %30: index, %31: index):
// P3-NEXT:    %32 = "llvm.icmp"(%28, %2) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%32, %28, %29, %30, %31, %29)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 4, 1>}> : (i1, index, f32, index, index, f32) -> ()
// P3-NEXT:  ^bb1(%33: index, %34: f32, %35: index, %36: index):
// P3-NEXT:    %37 = "llvm.mul"(%33, %35) : (index, index) -> index
// P3-NEXT:    %38 = "llvm.extractvalue"(%27) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// P3-NEXT:    "llvm.br"(%33, %4, %34, %37, %38, %35, %36)[^bb3] : (index, index, f32, index, !llvm.ptr, index, index) -> ()
// P3-NEXT:  ^bb3(%39: index, %40: index, %41: f32, %42: index, %43: !llvm.ptr, %44: index, %45: index):
// P3-NEXT:    %46 = "llvm.icmp"(%40, %3) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%46, %39, %40, %41, %42, %43, %44, %45, %39, %41, %44, %45)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 7, 4>}> : (i1, index, index, f32, index, !llvm.ptr, index, index, index, f32, index, index) -> ()
// P3-NEXT:  ^bb4(%47: index, %48: index, %49: f32, %50: index, %51: !llvm.ptr, %52: index, %53: index):
// P3-NEXT:    %54 = "llvm.mul"(%48, %53) : (index, index) -> index
// P3-NEXT:    %55 = "llvm.add"(%50, %54) : (index, index) -> index
// P3-NEXT:    %56 = "llvm.getelementptr"(%51, %55) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// P3-NEXT:    %57 = llvm.load %56 : !llvm.ptr -> f32
// P3-NEXT:    %58 = "llvm.fadd"(%49, %57) : (f32, f32) -> f32
// P3-NEXT:    %59 = "llvm.add"(%48, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%47, %59, %58, %50, %51, %52, %53)[^bb3] : (index, index, f32, index, !llvm.ptr, index, index) -> ()
// P3-NEXT:  ^bb5(%60: index, %61: f32, %62: index, %63: index):
// P3-NEXT:    %64 = "llvm.add"(%60, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%64, %61, %62, %63)[^bb0] : (index, f32, index, index) -> ()
// P3-NEXT:  ^bb2(%65: f32):
// P3-NEXT:    %66 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P3-NEXT:    "llvm.call"(%66) <{callee = @free}> : (!llvm.ptr) -> ()
// P3-NEXT:    "llvm.return"(%65) : (f32) -> ()
// P3-NEXT:  }

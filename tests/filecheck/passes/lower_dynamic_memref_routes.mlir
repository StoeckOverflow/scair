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
// P2-NEXT:    %18 = llvm.mlir.constant 256 : index : index
// P2-NEXT:    %19 = llvm.mlir.constant 1024 : index : index
// P2-NEXT:    %20 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P2-NEXT:    %21 = "llvm.extractvalue"(%17) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P2-NEXT:    %22 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %23 = "llvm.insertvalue"(%20, %22) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %24 = "llvm.insertvalue"(%21, %23) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %25 = "llvm.insertvalue"(%4, %24) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %26 = "llvm.insertvalue"(%18, %25) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %27 = "llvm.insertvalue"(%19, %26) <{position = array<i32: 3, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %28 = "llvm.insertvalue"(%0, %27) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    %29 = "llvm.insertvalue"(%1, %28) <{position = array<i32: 4, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P2-NEXT:    "llvm.br"(%4, %5)[^bb0] : (index, f32) -> ()
// P2-NEXT:  ^bb0(%30: index, %31: f32):
// P2-NEXT:    %32 = "llvm.icmp"(%30, %2) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%32, %30, %31, %31)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 2, 1>}> : (i1, index, f32, f32) -> ()
// P2-NEXT:  ^bb1(%33: index, %34: f32):
// P2-NEXT:    "llvm.br"(%33, %4, %34)[^bb3] : (index, index, f32) -> ()
// P2-NEXT:  ^bb3(%35: index, %36: index, %37: f32):
// P2-NEXT:    %38 = "llvm.icmp"(%36, %3) <{predicate = "slt"}> : (index, index) -> i1
// P2-NEXT:    "llvm.cond_br"(%38, %35, %36, %37, %35, %37)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 3, 2>}> : (i1, index, index, f32, index, f32) -> ()
// P2-NEXT:  ^bb4(%39: index, %40: index, %41: f32):
// P2-NEXT:    %42 = "llvm.extractvalue"(%29) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// P2-NEXT:    %43 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P2-NEXT:    %44 = "llvm.mul"(%39, %43) : (index, index) -> index
// P2-NEXT:    %45 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> index
// P2-NEXT:    %46 = "llvm.mul"(%40, %45) : (index, index) -> index
// P2-NEXT:    %47 = "llvm.add"(%44, %46) : (index, index) -> index
// P2-NEXT:    %48 = "llvm.getelementptr"(%42, %47) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// P2-NEXT:    %49 = llvm.load %48 : !llvm.ptr -> f32
// P2-NEXT:    %50 = "llvm.fadd"(%41, %49) : (f32, f32) -> f32
// P2-NEXT:    %51 = "llvm.add"(%40, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%39, %51, %50)[^bb3] : (index, index, f32) -> ()
// P2-NEXT:  ^bb5(%52: index, %53: f32):
// P2-NEXT:    %54 = "llvm.add"(%52, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P2-NEXT:    "llvm.br"(%54, %53)[^bb0] : (index, f32) -> ()
// P2-NEXT:  ^bb2(%55: f32):
// P2-NEXT:    %56 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P2-NEXT:    "llvm.call"(%56) <{callee = @free}> : (!llvm.ptr) -> ()
// P2-NEXT:    "llvm.return"(%55) : (f32) -> ()
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
// P3-NEXT:    %18 = llvm.mlir.constant 256 : index : index
// P3-NEXT:    %19 = llvm.mlir.constant 1024 : index : index
// P3-NEXT:    %20 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P3-NEXT:    %21 = "llvm.extractvalue"(%17) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P3-NEXT:    %22 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %23 = "llvm.insertvalue"(%20, %22) <{position = array<i32: 0>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %24 = "llvm.insertvalue"(%21, %23) <{position = array<i32: 1>}> : (!llvm.ptr, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %25 = "llvm.insertvalue"(%4, %24) <{position = array<i32: 2>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %26 = "llvm.insertvalue"(%18, %25) <{position = array<i32: 3, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %27 = "llvm.insertvalue"(%19, %26) <{position = array<i32: 3, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %28 = "llvm.insertvalue"(%0, %27) <{position = array<i32: 4, 0>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    %29 = "llvm.insertvalue"(%1, %28) <{position = array<i32: 4, 1>}> : (index, !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// P3-NEXT:    "llvm.br"(%4, %5, %0, %1)[^bb0] : (index, f32, index, index) -> ()
// P3-NEXT:  ^bb0(%30: index, %31: f32, %32: index, %33: index):
// P3-NEXT:    %34 = "llvm.icmp"(%30, %2) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%34, %30, %31, %32, %33, %31)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 4, 1>}> : (i1, index, f32, index, index, f32) -> ()
// P3-NEXT:  ^bb1(%35: index, %36: f32, %37: index, %38: index):
// P3-NEXT:    %39 = "llvm.mul"(%35, %37) : (index, index) -> index
// P3-NEXT:    %40 = "llvm.extractvalue"(%29) <{position = array<i32: 1>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>) -> !llvm.ptr
// P3-NEXT:    "llvm.br"(%35, %4, %36, %39, %40, %37, %38)[^bb3] : (index, index, f32, index, !llvm.ptr, index, index) -> ()
// P3-NEXT:  ^bb3(%41: index, %42: index, %43: f32, %44: index, %45: !llvm.ptr, %46: index, %47: index):
// P3-NEXT:    %48 = "llvm.icmp"(%42, %3) <{predicate = "slt"}> : (index, index) -> i1
// P3-NEXT:    "llvm.cond_br"(%48, %41, %42, %43, %44, %45, %46, %47, %41, %43, %46, %47)[^bb4, ^bb5] <{operandSegmentSizes = array<i32: 1, 7, 4>}> : (i1, index, index, f32, index, !llvm.ptr, index, index, index, f32, index, index) -> ()
// P3-NEXT:  ^bb4(%49: index, %50: index, %51: f32, %52: index, %53: !llvm.ptr, %54: index, %55: index):
// P3-NEXT:    %56 = "llvm.mul"(%50, %55) : (index, index) -> index
// P3-NEXT:    %57 = "llvm.add"(%4, %52) : (index, index) -> index
// P3-NEXT:    %58 = "llvm.add"(%57, %56) : (index, index) -> index
// P3-NEXT:    %59 = "llvm.getelementptr"(%53, %58) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// P3-NEXT:    %60 = llvm.load %59 : !llvm.ptr -> f32
// P3-NEXT:    %61 = "llvm.fadd"(%51, %60) : (f32, f32) -> f32
// P3-NEXT:    %62 = "llvm.add"(%50, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%49, %62, %61, %52, %53, %54, %55)[^bb3] : (index, index, f32, index, !llvm.ptr, index, index) -> ()
// P3-NEXT:  ^bb5(%63: index, %64: f32, %65: index, %66: index):
// P3-NEXT:    %67 = "llvm.add"(%63, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// P3-NEXT:    "llvm.br"(%67, %64, %65, %66)[^bb0] : (index, f32, index, index) -> ()
// P3-NEXT:  ^bb2(%68: f32):
// P3-NEXT:    %69 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}> : (!llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>) -> !llvm.ptr
// P3-NEXT:    "llvm.call"(%69) <{callee = @free}> : (!llvm.ptr) -> ()
// P3-NEXT:    "llvm.return"(%68) : (f32) -> ()
// P3-NEXT:  }

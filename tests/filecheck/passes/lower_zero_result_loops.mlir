// RUN: scair-opt %s -p lower-refined-dmemref-to-llvm-baseline | filecheck %s --check-prefix=BASE
// RUN: scair-opt %s -p lower-refined-dmemref-to-llvm-optimized | filecheck %s --check-prefix=OPT

builtin.module {
  func.func @zero_result_nested() {
    %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %c64 = "arith.constant"() <{value = 64 : index}> : () -> index
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
    %n8 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %n64 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
    %flat = d_memref.alloc : () -> !d_memref.memref<[%n64], f32>
    %A = d_memref.reinterpret_cast %flat
      : !d_memref.memref<[%n64], f32> to !d_memref.memref<[%n8, %n8], f32, offset: %c0, strides: [%c8, %c1]>

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
        d_memref.store %f1, %A[%i, %j] : f32, !d_memref.memref<[%n8, %n8], f32, offset: %c0, strides: [%c8, %c1]>
        "affine.yield"() : () -> ()
      }) : (index, index) -> ()
      "affine.yield"() : () -> ()
    }) : (index, index) -> ()

    d_memref.dealloc %flat : !d_memref.memref<[%n64], f32>
    "func.return"() : () -> ()
  }

  func.func @zero_then_reduction() -> f32 {
    %c8 = "arith.constant"() <{value = 8 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %c64 = "arith.constant"() <{value = 64 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
    %n8 = "dtensor.nat.const"() <{value = 8 : i32}> : () -> !dtensor.nat
    %n64 = "dtensor.nat.const"() <{value = 64 : i32}> : () -> !dtensor.nat
    %flat = d_memref.alloc : () -> !d_memref.memref<[%n64], f32>
    %A = d_memref.reinterpret_cast %flat
      : !d_memref.memref<[%n64], f32> to !d_memref.memref<[%n8, %n8], f32, offset: %c0, strides: [%c8, %c1]>

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
        d_memref.store %f1, %A[%i, %j] : f32, !d_memref.memref<[%n8, %n8], f32, offset: %c0, strides: [%c8, %c1]>
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
        %x = d_memref.load %A[%i, %j] : !d_memref.memref<[%n8, %n8], f32, offset: %c0, strides: [%c8, %c1]> -> f32
        %y = "arith.addf"(%acc2, %x) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        "affine.yield"(%y) : (f32) -> ()
      }) : (index, index, f32) -> f32
      "affine.yield"(%inner) : (f32) -> ()
    }) : (index, index, f32) -> f32

    d_memref.dealloc %flat : !d_memref.memref<[%n64], f32>
    "func.return"(%sum) : (f32) -> ()
  }
}

// -----------------------------------------------------------------------------
// zero-result nested loops keep a real outer loop + inner loop, and the
// inner body performs the store before back-branching with live block args.
// -----------------------------------------------------------------------------

// BASE-LABEL: func.func @zero_result_nested() {
// BASE: %0 = llvm.mlir.constant 8 : index : index
// BASE: %1 = llvm.mlir.constant 0 : index : index
// BASE: %2 = llvm.mlir.constant 1 : index : index
// BASE: %4 = llvm.mlir.constant 1.0 : f32 : f32
// BASE: "llvm.br"(%1)[^bb0] : (index) -> ()
// BASE: ^bb0(%28: index):
// BASE: %29 = "llvm.icmp"(%28, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%29, %28)[^bb1, ^bb2]
// BASE: ^bb1(%30: index):
// BASE: "llvm.br"(%1, %30)[^bb3] : (index, index) -> ()
// BASE: ^bb3(%32: index, %33: index):
// BASE: %34 = "llvm.icmp"(%32, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%34, %32, %33, %33)[^bb4, ^bb5]
// BASE: ^bb4(%35: index, %36: index):
// BASE: %37 = "llvm.extractvalue"(%27) <{position = array<i32: 1>}>
// BASE: %38 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 0>}>
// BASE: %39 = "llvm.mul"(%36, %38) : (index, index) -> index
// BASE: %40 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 1>}>
// BASE: %41 = "llvm.mul"(%35, %40) : (index, index) -> index
// BASE: %42 = "llvm.add"(%39, %41) : (index, index) -> index
// BASE: %43 = "llvm.getelementptr"(%37, %42) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// BASE: llvm.store %4, %43 : f32, !llvm.ptr
// BASE: %44 = "llvm.add"(%35, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%44, %36)[^bb3] : (index, index) -> ()
// BASE: ^bb5(%45: index):
// BASE: %46 = "llvm.add"(%45, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%46)[^bb0] : (index) -> ()

// -----------------------------------------------------------------------------
// mixed zero-result init loop followed by reduction loop. The init loop stores
// 1.0, and the reduction loop threads the accumulator and does load+fadd.
// -----------------------------------------------------------------------------

// BASE-LABEL: func.func @zero_then_reduction() -> f32 {
// BASE: %0 = llvm.mlir.constant 8 : index : index
// BASE: %1 = llvm.mlir.constant 0 : index : index
// BASE: %2 = llvm.mlir.constant 1 : index : index
// BASE: %4 = llvm.mlir.constant 0.0 : f32 : f32
// BASE: %5 = llvm.mlir.constant 1.0 : f32 : f32
// BASE: "llvm.br"(%1)[^bb0] : (index) -> ()
// BASE: ^bb0(%29: index):
// BASE: %30 = "llvm.icmp"(%29, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%30, %29)[^bb1, ^bb2]
// BASE: ^bb1(%31: index):
// BASE: "llvm.br"(%1, %31)[^bb3] : (index, index) -> ()
// BASE: ^bb2:
// BASE: "llvm.br"(%1, %4)[^bb4] : (index, f32) -> ()
// BASE: ^bb3(%32: index, %33: index):
// BASE: %34 = "llvm.icmp"(%32, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%34, %32, %33, %33)[^bb5, ^bb6]
// BASE: ^bb5(%35: index, %36: index):
// BASE: %37 = "llvm.extractvalue"(%28) <{position = array<i32: 1>}>
// BASE: %38 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 0>}>
// BASE: %39 = "llvm.mul"(%36, %38) : (index, index) -> index
// BASE: %40 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 1>}>
// BASE: %41 = "llvm.mul"(%35, %40) : (index, index) -> index
// BASE: %42 = "llvm.add"(%39, %41) : (index, index) -> index
// BASE: %43 = "llvm.getelementptr"(%37, %42) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// BASE: llvm.store %5, %43 : f32, !llvm.ptr
// BASE: %44 = "llvm.add"(%35, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%44, %36)[^bb3] : (index, index) -> ()
// BASE: ^bb6(%45: index):
// BASE: %46 = "llvm.add"(%45, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%46)[^bb0] : (index) -> ()
// BASE: ^bb4(%47: index, %48: f32):
// BASE: %49 = "llvm.icmp"(%47, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%49, %47, %48, %48)[^bb7, ^bb8]
// BASE: ^bb7(%50: index, %51: f32):
// BASE: "llvm.br"(%50, %1, %51)[^bb9] : (index, index, f32) -> ()
// BASE: ^bb9(%52: index, %53: index, %54: f32):
// BASE: %55 = "llvm.icmp"(%53, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%55, %52, %53, %54, %52, %54)[^bb10, ^bb11]
// BASE: ^bb10(%56: index, %57: index, %58: f32):
// BASE: %59 = "llvm.extractvalue"(%28) <{position = array<i32: 1>}>
// BASE: %60 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 0>}>
// BASE: %61 = "llvm.mul"(%56, %60) : (index, index) -> index
// BASE: %62 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 1>}>
// BASE: %63 = "llvm.mul"(%57, %62) : (index, index) -> index
// BASE: %64 = "llvm.add"(%61, %63) : (index, index) -> index
// BASE: %65 = "llvm.getelementptr"(%59, %64) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// BASE: %66 = llvm.load %65 : !llvm.ptr -> f32
// BASE: %67 = "llvm.fadd"(%58, %66) : (f32, f32) -> f32
// BASE: %68 = "llvm.add"(%57, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%56, %68, %67)[^bb9] : (index, index, f32) -> ()
// BASE: ^bb11(%69: index, %70: f32):
// BASE: %71 = "llvm.add"(%69, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%71, %70)[^bb4] : (index, f32) -> ()

// -----------------------------------------------------------------------------
// optimized zero-result nested loops keep explicit outer/inner CFG, with stores
// on a flat pointer and backedges carrying the live row-base/IV values.
// -----------------------------------------------------------------------------

// OPT-LABEL: func.func @zero_result_nested() {
// OPT: %0 = llvm.mlir.constant 1 : index : index
// OPT: %1 = llvm.mlir.constant 8 : index : index
// OPT: %2 = llvm.mlir.constant 0 : index : index
// OPT: %3 = llvm.mlir.constant 1.0 : f32 : f32
// OPT: "llvm.br"(%2)[^bb0] : (index) -> ()
// OPT: ^bb0(%9: index):
// OPT: %10 = "llvm.icmp"(%9, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%10, %9)[^bb1, ^bb2]
// OPT: ^bb1(%11: index):
// OPT: %12 = "llvm.mul"(%11, %1) : (index, index) -> index
// OPT: "llvm.br"(%2, %12, %11)[^bb3] : (index, index, index) -> ()
// OPT: ^bb3(%13: index, %14: index, %15: index):
// OPT: %16 = "llvm.icmp"(%13, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%16, %13, %14, %15, %14, %15)[^bb4, ^bb5]
// OPT: ^bb4(%17: index, %18: index, %19: index):
// OPT: %20 = "llvm.mul"(%17, %0) : (index, index) -> index
// OPT: %21 = "llvm.add"(%2, %18) : (index, index) -> index
// OPT: %22 = "llvm.add"(%21, %20) : (index, index) -> index
// OPT: %23 = "llvm.getelementptr"(%8, %22) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// OPT: llvm.store %3, %23 : f32, !llvm.ptr
// OPT: %24 = "llvm.add"(%17, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%24, %18, %19)[^bb3] : (index, index, index) -> ()
// OPT: ^bb5(%25: index, %26: index):
// OPT: %27 = "llvm.add"(%25, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%27, %26)[^bb0] : (index, index) -> ()

// -----------------------------------------------------------------------------
// optimized mixed init + reduction keeps the init-store nest and then lowers
// the reduction to load+fadd with an accumulator threaded through block args.
// -----------------------------------------------------------------------------

// OPT-LABEL: func.func @zero_then_reduction() -> f32 {
// OPT: %0 = llvm.mlir.constant 1 : index : index
// OPT: %1 = llvm.mlir.constant 8 : index : index
// OPT: %2 = llvm.mlir.constant 0 : index : index
// OPT: %3 = llvm.mlir.constant 0.0 : f32 : f32
// OPT: %4 = llvm.mlir.constant 1.0 : f32 : f32
// OPT: "llvm.br"(%2)[^bb0] : (index) -> ()
// OPT: ^bb0(%10: index):
// OPT: %11 = "llvm.icmp"(%10, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%11, %10)[^bb1, ^bb2]
// OPT: ^bb1(%12: index):
// OPT: %13 = "llvm.mul"(%12, %1) : (index, index) -> index
// OPT: "llvm.br"(%2, %13, %12)[^bb3] : (index, index, index) -> ()
// OPT: ^bb2:
// OPT: "llvm.br"(%2, %3)[^bb4] : (index, f32) -> ()
// OPT: ^bb3(%14: index, %15: index, %16: index):
// OPT: %17 = "llvm.icmp"(%14, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%17, %14, %15, %16, %15, %16)[^bb5, ^bb6]
// OPT: ^bb5(%18: index, %19: index, %20: index):
// OPT: %21 = "llvm.mul"(%18, %0) : (index, index) -> index
// OPT: %22 = "llvm.add"(%2, %19) : (index, index) -> index
// OPT: %23 = "llvm.add"(%22, %21) : (index, index) -> index
// OPT: %24 = "llvm.getelementptr"(%9, %23) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// OPT: llvm.store %4, %24 : f32, !llvm.ptr
// OPT: %25 = "llvm.add"(%18, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%25, %19, %20)[^bb3] : (index, index, index) -> ()
// OPT: ^bb6(%26: index, %27: index):
// OPT: %28 = "llvm.add"(%26, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%28, %27)[^bb0] : (index, index) -> ()
// OPT: ^bb4(%29: index, %30: f32):
// OPT: %31 = "llvm.icmp"(%29, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%31, %29, %30, %30)[^bb7, ^bb8]
// OPT: ^bb7(%32: index, %33: f32):
// OPT: %34 = "llvm.mul"(%32, %1) : (index, index) -> index
// OPT: "llvm.br"(%32, %2, %33, %34)[^bb9] : (index, index, f32, index) -> ()
// OPT: ^bb9(%35: index, %36: index, %37: f32, %38: index):
// OPT: %39 = "llvm.icmp"(%36, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%39, %35, %36, %37, %38, %35, %37)[^bb10, ^bb11]
// OPT: ^bb10(%40: index, %41: index, %42: f32, %43: index):
// OPT: %44 = "llvm.mul"(%41, %0) : (index, index) -> index
// OPT: %45 = "llvm.add"(%2, %43) : (index, index) -> index
// OPT: %46 = "llvm.add"(%45, %44) : (index, index) -> index
// OPT: %47 = "llvm.getelementptr"(%9, %46) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// OPT: %48 = llvm.load %47 : !llvm.ptr -> f32
// OPT: %49 = "llvm.fadd"(%42, %48) : (f32, f32) -> f32
// OPT: %50 = "llvm.add"(%41, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%40, %50, %49, %43)[^bb9] : (index, index, f32, index) -> ()
// OPT: ^bb11(%51: index, %52: f32):
// OPT: %53 = "llvm.add"(%51, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%53, %52)[^bb4] : (index, f32) -> ()

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
// BASE: "llvm.cond_br"(%29)[^bb1, ^bb2]
// BASE: ^bb1:
// BASE: "llvm.br"(%1)[^bb3] : (index) -> ()
// BASE: ^bb3(%31: index):
// BASE: %32 = "llvm.icmp"(%31, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%32)[^bb4, ^bb5]
// BASE: ^bb4:
// BASE: %33 = "llvm.extractvalue"(%27) <{position = array<i32: 1>}>
// BASE: %34 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 0>}>
// BASE: %35 = "llvm.mul"(%28, %34) : (index, index) -> index
// BASE: %36 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 1>}>
// BASE: %37 = "llvm.mul"(%31, %36) : (index, index) -> index
// BASE: %38 = "llvm.add"(%35, %37) : (index, index) -> index
// BASE: %39 = "llvm.getelementptr"(%33, %38) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// BASE: llvm.store %4, %39 : f32, !llvm.ptr
// BASE: %40 = "llvm.add"(%31, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%40)[^bb3] : (index) -> ()
// BASE: ^bb5:
// BASE: %41 = "llvm.add"(%28, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%41)[^bb0] : (index) -> ()

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
// BASE: "llvm.cond_br"(%30)[^bb1, ^bb2]
// BASE: ^bb1:
// BASE: "llvm.br"(%1)[^bb3] : (index) -> ()
// BASE: ^bb2:
// BASE: "llvm.br"(%1, %4)[^bb4] : (index, f32) -> ()
// BASE: ^bb3(%31: index):
// BASE: %32 = "llvm.icmp"(%31, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%32)[^bb5, ^bb6]
// BASE: ^bb5:
// BASE: %33 = "llvm.extractvalue"(%28) <{position = array<i32: 1>}>
// BASE: %34 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 0>}>
// BASE: %35 = "llvm.mul"(%29, %34) : (index, index) -> index
// BASE: %36 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 1>}>
// BASE: %37 = "llvm.mul"(%31, %36) : (index, index) -> index
// BASE: %38 = "llvm.add"(%35, %37) : (index, index) -> index
// BASE: %39 = "llvm.getelementptr"(%33, %38) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// BASE: llvm.store %5, %39 : f32, !llvm.ptr
// BASE: %40 = "llvm.add"(%31, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%40)[^bb3] : (index) -> ()
// BASE: ^bb6:
// BASE: %41 = "llvm.add"(%29, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%41)[^bb0] : (index) -> ()
// BASE: ^bb4(%42: index, %43: f32):
// BASE: %44 = "llvm.icmp"(%42, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%44)[^bb7, ^bb8]
// BASE: ^bb7:
// BASE: "llvm.br"(%42, %1, %43)[^bb9] : (index, index, f32) -> ()
// BASE: ^bb9(%45: index, %46: index, %47: f32):
// BASE: %48 = "llvm.icmp"(%46, %0) <{predicate = "slt"}> : (index, index) -> i1
// BASE: "llvm.cond_br"(%48)[^bb10, ^bb11]
// BASE: ^bb10:
// BASE: %49 = "llvm.extractvalue"(%28) <{position = array<i32: 1>}>
// BASE: %50 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 0>}>
// BASE: %51 = "llvm.mul"(%45, %50) : (index, index) -> index
// BASE: %52 = "llvm.extractvalue"(%28) <{position = array<i32: 4, 1>}>
// BASE: %53 = "llvm.mul"(%46, %52) : (index, index) -> index
// BASE: %54 = "llvm.add"(%51, %53) : (index, index) -> index
// BASE: %55 = "llvm.getelementptr"(%49, %54) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// BASE: %56 = llvm.load %55 : !llvm.ptr -> f32
// BASE: %57 = "llvm.fadd"(%47, %56) : (f32, f32) -> f32
// BASE: %58 = "llvm.add"(%46, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%45, %58, %57)[^bb9] : (index, index, f32) -> ()
// BASE: ^bb11:
// BASE: %59 = "llvm.add"(%45, %2) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// BASE: "llvm.br"(%59, %47)[^bb4] : (index, f32) -> ()

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
// OPT: "llvm.cond_br"(%10)[^bb1, ^bb2]
// OPT: ^bb1:
// OPT: %11 = "llvm.mul"(%9, %1) : (index, index) -> index
// OPT: "llvm.br"(%2)[^bb3] : (index) -> ()
// OPT: ^bb3(%12: index):
// OPT: %13 = "llvm.icmp"(%12, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%13)[^bb4, ^bb5]
// OPT: ^bb4:
// OPT: %14 = "llvm.mul"(%12, %0) : (index, index) -> index
// OPT: %15 = "llvm.add"(%2, %11) : (index, index) -> index
// OPT: %16 = "llvm.add"(%15, %14) : (index, index) -> index
// OPT: %17 = "llvm.getelementptr"(%8, %16) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// OPT: llvm.store %3, %17 : f32, !llvm.ptr
// OPT: %18 = "llvm.add"(%12, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%18)[^bb3] : (index) -> ()
// OPT: ^bb5:
// OPT: %19 = "llvm.add"(%9, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%19)[^bb0] : (index) -> ()

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
// OPT: "llvm.cond_br"(%11)[^bb1, ^bb2]
// OPT: ^bb1:
// OPT: %12 = "llvm.mul"(%10, %1) : (index, index) -> index
// OPT: "llvm.br"(%2)[^bb3] : (index) -> ()
// OPT: ^bb2:
// OPT: "llvm.br"(%2, %3)[^bb4] : (index, f32) -> ()
// OPT: ^bb3(%13: index):
// OPT: %14 = "llvm.icmp"(%13, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%14)[^bb5, ^bb6]
// OPT: ^bb5:
// OPT: %15 = "llvm.mul"(%13, %0) : (index, index) -> index
// OPT: %16 = "llvm.add"(%2, %12) : (index, index) -> index
// OPT: %17 = "llvm.add"(%16, %15) : (index, index) -> index
// OPT: %18 = "llvm.getelementptr"(%9, %17) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// OPT: llvm.store %4, %18 : f32, !llvm.ptr
// OPT: %19 = "llvm.add"(%13, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%19)[^bb3] : (index) -> ()
// OPT: ^bb6:
// OPT: %20 = "llvm.add"(%10, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%20)[^bb0] : (index) -> ()
// OPT: ^bb4(%21: index, %22: f32):
// OPT: %23 = "llvm.icmp"(%21, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%23)[^bb7, ^bb8]
// OPT: ^bb7:
// OPT: %24 = "llvm.mul"(%21, %1) : (index, index) -> index
// OPT: "llvm.br"(%21, %2, %22, %24)[^bb9] : (index, index, f32, index) -> ()
// OPT: ^bb9(%25: index, %26: index, %27: f32, %28: index):
// OPT: %29 = "llvm.icmp"(%26, %1) <{predicate = "slt"}> : (index, index) -> i1
// OPT: "llvm.cond_br"(%29)[^bb10, ^bb11]
// OPT: ^bb10:
// OPT: %30 = "llvm.mul"(%26, %0) : (index, index) -> index
// OPT: %31 = "llvm.add"(%2, %28) : (index, index) -> index
// OPT: %32 = "llvm.add"(%31, %30) : (index, index) -> index
// OPT: %33 = "llvm.getelementptr"(%9, %32) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// OPT: %34 = llvm.load %33 : !llvm.ptr -> f32
// OPT: %35 = "llvm.fadd"(%27, %34) : (f32, f32) -> f32
// OPT: %36 = "llvm.add"(%26, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%25, %36, %35, %28)[^bb9] : (index, index, f32, index) -> ()
// OPT: ^bb11:
// OPT: %37 = "llvm.add"(%25, %0) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// OPT: "llvm.br"(%37, %27)[^bb4] : (index, f32) -> ()

// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm-baseline | filecheck %s --check-prefix=P1
// RUN: scair-opt %s -p lower-dynamic-memref-to-llvm | filecheck %s --check-prefix=P2
// RUN: scair-opt %S/lower_refined_dmemref_gemm_to_llvm.mlir -p lower-dmemref-to-llvm | filecheck %s --check-prefix=P3

builtin.module {
  func.func @matmul_dynamic(
    %n : index,
    %m : index,
    %k : index,
    %Aflat : memref<?xf32>,
    %Bflat : memref<?xf32>,
    %Cflat : memref<?xf32>
  ) {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %A = "memref.reinterpret_cast"(%Aflat, %c0, %n, %k, %k, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>
    %B = "memref.reinterpret_cast"(%Bflat, %c0, %k, %m, %m, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>
    %C = "memref.reinterpret_cast"(%Cflat, %c0, %n, %m, %m, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>

    "affine.for"(%c0, %n) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 0>
    }> ({
    ^bb0(%i: index):
      "affine.for"(%c0, %m) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 0>
      }> ({
      ^bb0(%j: index):
        %sum = "affine.for"(%c0, %k, %f0) <{
          lowerBoundMap = affine_map<(d0) -> (d0)>,
          upperBoundMap = affine_map<(d0) -> (d0)>,
          step = 1 : index,
          operandSegmentSizes = array<i32: 1, 1, 1>
        }> ({
        ^bb0(%p: index, %acc: f32):
          %a = "memref.load"(%A, %i, %p)
            : (memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
          %b = "memref.load"(%B, %p, %j)
            : (memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
          %mul = "arith.mulf"(%a, %b) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          %next = "arith.addf"(%acc, %mul) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
          "affine.yield"(%next) : (f32) -> ()
        }) : (index, index, f32) -> f32
        "memref.store"(%sum, %C, %i, %j)
          : (f32, memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
        "affine.yield"() : () -> ()
      }) : (index, index) -> ()
      "affine.yield"() : () -> ()
    }) : (index, index) -> ()

    "func.return"() : () -> ()
  }

  func.func @checksum_dynamic(
    %n : index,
    %m : index,
    %Cflat : memref<?xf32>
  ) -> f32 {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %C = "memref.reinterpret_cast"(%Cflat, %c0, %n, %m, %m, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>

    %result = "affine.for"(%c0, %n, %f0) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 1>
    }> ({
    ^bb0(%i: index, %acc: f32):
      %inner = "affine.for"(%c0, %m, %acc) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 1>
      }> ({
      ^bb0(%j: index, %acc2: f32):
        %v = "memref.load"(%C, %i, %j)
          : (memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> f32
        %next = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        "affine.yield"(%next) : (f32) -> ()
      }) : (index, index, f32) -> f32
      "affine.yield"(%inner) : (f32) -> ()
    }) : (index, index, f32) -> f32

    "func.return"(%result) : (f32) -> ()
  }

  func.func private @printF32(f32)
  func.func private @printNewline()

  func.func @main() -> i32 {
    %n = "arith.constant"() <{value = 32 : index}> : () -> index
    %m = "arith.constant"() <{value = 32 : index}> : () -> index
    %k = "arith.constant"() <{value = 32 : index}> : () -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32
    %ret = "arith.constant"() <{value = 0 : i32}> : () -> i32

    %A_size = "arith.muli"(%n, %k) : (index, index) -> index
    %B_size = "arith.muli"(%k, %m) : (index, index) -> index
    %C_size = "arith.muli"(%n, %m) : (index, index) -> index

    %Aflat = "memref.alloc"(%A_size) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}>
      : (index) -> memref<?xf32>
    %Bflat = "memref.alloc"(%B_size) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}>
      : (index) -> memref<?xf32>
    %Cflat = "memref.alloc"(%C_size) <{alignment = 0 : i64, operandSegmentSizes = array<i32: 1, 0>}>
      : (index) -> memref<?xf32>

    %A = "memref.reinterpret_cast"(%Aflat, %c0, %n, %k, %k, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>
    %B = "memref.reinterpret_cast"(%Bflat, %c0, %k, %m, %m, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>
    %C = "memref.reinterpret_cast"(%Cflat, %c0, %n, %m, %m, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 2, 2>}>
      : (memref<?xf32>, index, index, index, index, index)
        -> memref<?x?xf32, strided<[?, ?], offset: 0>>

    "affine.for"(%c0, %n) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 0>
    }> ({
    ^bb0(%i: index):
      "affine.for"(%c0, %k) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 0>
      }> ({
      ^bb0(%j: index):
        "memref.store"(%f1, %A, %i, %j)
          : (f32, memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
        "affine.yield"() : () -> ()
      }) : (index, index) -> ()
      "affine.yield"() : () -> ()
    }) : (index, index) -> ()

    "affine.for"(%c0, %k) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 0>
    }> ({
    ^bb0(%i: index):
      "affine.for"(%c0, %m) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 0>
      }> ({
      ^bb0(%j: index):
        "memref.store"(%f1, %B, %i, %j)
          : (f32, memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
        "affine.yield"() : () -> ()
      }) : (index, index) -> ()
      "affine.yield"() : () -> ()
    }) : (index, index) -> ()

    "affine.for"(%c0, %n) <{
      lowerBoundMap = affine_map<(d0) -> (d0)>,
      upperBoundMap = affine_map<(d0) -> (d0)>,
      step = 1 : index,
      operandSegmentSizes = array<i32: 1, 1, 0>
    }> ({
    ^bb0(%i: index):
      "affine.for"(%c0, %m) <{
        lowerBoundMap = affine_map<(d0) -> (d0)>,
        upperBoundMap = affine_map<(d0) -> (d0)>,
        step = 1 : index,
        operandSegmentSizes = array<i32: 1, 1, 0>
      }> ({
      ^bb0(%j: index):
        "memref.store"(%f0, %C, %i, %j)
          : (f32, memref<?x?xf32, strided<[?, ?], offset: 0>>, index, index) -> ()
        "affine.yield"() : () -> ()
      }) : (index, index) -> ()
      "affine.yield"() : () -> ()
    }) : (index, index) -> ()

    "func.call"(%n, %m, %k, %Aflat, %Bflat, %Cflat) <{callee = @matmul_dynamic}>
      : (index, index, index, memref<?xf32>, memref<?xf32>, memref<?xf32>) -> ()
    %checksum = "func.call"(%n, %m, %Cflat) <{callee = @checksum_dynamic}>
      : (index, index, memref<?xf32>) -> f32
    "func.call"(%checksum) <{callee = @printF32}> : (f32) -> ()
    "func.call"() <{callee = @printNewline}> : () -> ()

    "memref.dealloc"(%Aflat) : (memref<?xf32>) -> ()
    "memref.dealloc"(%Bflat) : (memref<?xf32>) -> ()
    "memref.dealloc"(%Cflat) : (memref<?xf32>) -> ()
    "func.return"(%ret) : (i32) -> ()
  }
}

// P1-LABEL: func.func @matmul_dynamic(%0: i64, %1: i64, %2: i64, %3: !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>, %4: !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>, %5: !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>) {
// P1-NEXT:    %6 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P1-NEXT:    %7 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P1-NEXT:    %8 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P1-NEXT:    %9 = llvm.extractvalue %3[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %10 = llvm.extractvalue %3[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>
// P1-NEXT:    %11 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %12 = llvm.insertvalue %9, %11[0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %13 = llvm.insertvalue %10, %12[1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %14 = llvm.insertvalue %6, %13[2] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %15 = llvm.insertvalue %0, %14[3, 0] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1-NEXT:    %16 = llvm.insertvalue %2, %15[3, 1] : !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<2 x i64>, !llvm.array<2 x i64>)>
// P1:         %40 = llvm.icmp "slt" %39, %0 : i64
// P1-NEXT:    "llvm.cond_br"(%40)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P1:         %54 = "llvm.getelementptr"(%48, %53) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P1-NEXT:    %55 = llvm.load %54 : !llvm.ptr -> f32
// P1:         %74 = "llvm.getelementptr"(%68, %73) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P1-NEXT:    "llvm.store"(%46, %74) : (f32, !llvm.ptr) -> ()

// P1-LABEL: func.func @main() -> i32 {
// P1-NEXT:    %0 = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// P1-NEXT:    %1 = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// P1-NEXT:    %2 = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// P1-NEXT:    %3 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P1-NEXT:    %4 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P1-NEXT:    %5 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P1-NEXT:    %6 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// P1-NEXT:    %7 = "llvm.mlir.constant"() <{value = 0 : i32}> : () -> i32
// P1-NEXT:    %8 = "llvm.mul"(%0, %2) : (i64, i64) -> i64
// P1-NEXT:    %9 = "llvm.mul"(%2, %1) : (i64, i64) -> i64
// P1-NEXT:    %10 = "llvm.mul"(%0, %1) : (i64, i64) -> i64
// P1:         "func.call"(%0, %1, %2, %20, %30, %40) <{callee = @matmul_dynamic}> : (i64, i64, i64, !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>, !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>, !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>) -> ()
// P1-NEXT:    %103 = "func.call"(%0, %1, %40) <{callee = @checksum_dynamic}> : (i64, i64, !llvm.struct<(!llvm.ptr, !llvm.ptr, i64, !llvm.array<1 x i64>, !llvm.array<1 x i64>)>) -> f32

// P2-LABEL: func.func @matmul_dynamic(%0: i64, %1: i64, %2: i64, %3: i64, %4: !llvm.ptr, %5: i64, %6: !llvm.ptr, %7: i64, %8: !llvm.ptr) {
// P2-NEXT:    %9 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P2-NEXT:    %10 = "llvm.add"(%0, %9) : (i64, i64) -> i64
// P2-NEXT:    %11 = "llvm.add"(%1, %9) : (i64, i64) -> i64
// P2-NEXT:    %12 = "llvm.add"(%2, %9) : (i64, i64) -> i64
// P2-NEXT:    %13 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P2-NEXT:    %14 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P2-NEXT:    %15 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P2-NEXT:    "llvm.br"(%14)[^bb0] : (i64) -> ()
// P2-NEXT:  ^bb0(%16: i64):
// P2-NEXT:    %17 = llvm.icmp "slt" %16, %10 : i64
// P2-NEXT:    "llvm.cond_br"(%17)[^bb1, ^bb2] <{operandSegmentSizes = array<i32: 1, 0, 0>}> : (i1) -> ()
// P2:         %28 = "llvm.getelementptr"(%4, %27) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P2-NEXT:    %29 = llvm.load %28 : !llvm.ptr -> f32
// P2:         %43 = "llvm.getelementptr"(%8, %42) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P2-NEXT:    "llvm.store"(%22, %43) : (f32, !llvm.ptr) -> ()

// P2-LABEL: func.func @main() -> i32 {
// P2-NEXT:    %0 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P2-NEXT:    %1 = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// P2-NEXT:    %2 = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// P2-NEXT:    %3 = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// P2-NEXT:    %4 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P2-NEXT:    %5 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P2-NEXT:    %6 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// P2-NEXT:    %7 = "llvm.mlir.constant"() <{value = 0 : i32}> : () -> i32
// P2-NEXT:    %8 = "llvm.mul"(%1, %3) : (i64, i64) -> i64
// P2-NEXT:    %9 = "llvm.mul"(%3, %2) : (i64, i64) -> i64
// P2-NEXT:    %10 = "llvm.mul"(%1, %2) : (i64, i64) -> i64
// P2:         "func.call"(%1, %2, %3, %8, %14, %9, %18, %10, %22) <{callee = @matmul_dynamic}> : (i64, i64, i64, i64, !llvm.ptr, i64, !llvm.ptr, i64, !llvm.ptr) -> ()
// P2-NEXT:    %47 = "func.call"(%1, %2, %10, %22) <{callee = @checksum_dynamic}> : (i64, i64, i64, !llvm.ptr) -> f32

// P3-LABEL: func.func @main() -> i32 {
// P3-NEXT:    %0 = "llvm.mlir.constant"() <{value = 1}> : () -> i64
// P3-NEXT:    %1 = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// P3-NEXT:    %2 = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// P3-NEXT:    %3 = "llvm.mlir.constant"() <{value = 32}> : () -> i64
// P3-NEXT:    %4 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// P3-NEXT:    %5 = "llvm.mlir.constant"() <{value = 0.0 : f32}> : () -> f32
// P3-NEXT:    %6 = "llvm.mlir.constant"() <{value = 1.0 : f32}> : () -> f32
// P3-NEXT:    %7 = "llvm.mlir.constant"() <{value = 0 : i32}> : () -> i32
// P3-NEXT:    %8 = "llvm.mul"(%1, %3) : (i64, i64) -> i64
// P3-NEXT:    %9 = "llvm.mul"(%3, %2) : (i64, i64) -> i64
// P3-NEXT:    %10 = "llvm.mul"(%1, %2) : (i64, i64) -> i64
// P3-NEXT:    %11 = "llvm.mlir.zero"() : () -> !llvm.ptr
// P3-NEXT:    %12 = "llvm.getelementptr"(%11, %8) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// P3-NEXT:    %13 = "llvm.ptrtoint"(%12) : (!llvm.ptr) -> i64
// P3-NEXT:    %14 = llvm.call @malloc(%13) : (i64) -> !llvm.ptr
// P3:         %65 = "llvm.fmul"(%58, %64) : (f32, f32) -> f32
// P3-NEXT:    %66 = "llvm.fadd"(%51, %65) : (f32, f32) -> f32
// P3:         llvm.call @free(%14) : (!llvm.ptr) -> ()
// P3-NEXT:    llvm.call @free(%18) : (!llvm.ptr) -> ()
// P3-NEXT:    llvm.call @free(%22) : (!llvm.ptr) -> ()
// P3-NEXT:    "llvm.return"(%7) : (i32) -> ()

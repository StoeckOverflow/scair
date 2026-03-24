// RUN: scair-opt %s -p lower-refined-dmemref-to-llvm-pipeline | filecheck %s -dump-input=always

builtin.module {
  func.func @semi_affine_layout_map(%stride0 : index, %stride1 : index) -> f32 {

    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %total = "arith.muli"(%c256 , %stride0): (index, index) -> index
    %flat_nat = "dtensor.index_to_nat"(%total) : (index) -> !dtensor.nat
    %flat = d_memref.alloc : () -> !d_memref.memref<[%flat_nat], f32>

    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index

    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %d0 = "dtensor.nat.const"() <{value = 256 : i32}> : () -> !dtensor.nat
    %d1 = "dtensor.nat.const"() <{value = 1024 : i32}> : () -> !dtensor.nat
    
    %buf = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%flat_nat], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %zero, strides: [%stride0, %stride1]>
    
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %result = d_affine.for %i = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c256) step 1 : i32 iter_args(%acc = %cst : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c1024) step 1 : i32 iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %buf[%i, %j] : !d_memref.memref<[%d0, %d1], f32, offset: %zero, strides: [%stride0, %stride1]> -> f32
        %sum = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %sum : (f32)
      }
      d_affine.yield %inner : (f32)
    }
    d_memref.dealloc %flat : !d_memref.memref<[%flat_nat], f32>
    func.return %result : f32
  }
}

// CHECK-LABEL: func.func @semi_affine_layout_map(%0: index, %1: index) -> f32 {
// CHECK:         %2 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %3 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %4 = llvm.mlir.constant 0 : index : index
// CHECK-NEXT:    %5 = llvm.mlir.constant 0.0 : f32 : f32
// CHECK-NEXT:    %6 = "llvm.mul"(%2, %0) : (index, index) -> index
// CHECK-NEXT:    %7 = llvm.mlir.constant 1 : index : index
// CHECK-NEXT:    %8 = llvm.mlir.zero : !llvm.ptr
// CHECK-NEXT:    %9 = "llvm.getelementptr"(%8, %6) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %10 = "llvm.ptrtoint"(%9) : (!llvm.ptr) -> index
// CHECK-NEXT:    %11 = "llvm.call"(%10) <{callee = @malloc}> : (index) -> !llvm.ptr
// CHECK-NEXT:    %12 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// CHECK-NEXT:    %13 = "llvm.insertvalue"(%11, %12) <{position = array<i32: 0>}>
// CHECK-NEXT:    %14 = "llvm.insertvalue"(%11, %13) <{position = array<i32: 1>}>
// CHECK-NEXT:    %15 = "llvm.insertvalue"(%4, %14) <{position = array<i32: 2>}>
// CHECK-NEXT:    %16 = "llvm.insertvalue"(%6, %15) <{position = array<i32: 3, 0>}>
// CHECK-NEXT:    %17 = "llvm.insertvalue"(%7, %16) <{position = array<i32: 4, 0>}>
// CHECK-NEXT:    %18 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %19 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %20 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}>
// CHECK-NEXT:    %21 = "llvm.extractvalue"(%17) <{position = array<i32: 1>}>
// CHECK-NEXT:    %22 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %23 = "llvm.insertvalue"(%20, %22) <{position = array<i32: 0>}>
// CHECK-NEXT:    %24 = "llvm.insertvalue"(%21, %23) <{position = array<i32: 1>}>
// CHECK-NEXT:    %25 = "llvm.insertvalue"(%4, %24) <{position = array<i32: 2>}>
// CHECK-NEXT:    %26 = "llvm.insertvalue"(%18, %25) <{position = array<i32: 3, 0>}>
// CHECK-NEXT:    %27 = "llvm.insertvalue"(%19, %26) <{position = array<i32: 3, 1>}>
// CHECK-NEXT:    %28 = "llvm.insertvalue"(%0, %27) <{position = array<i32: 4, 0>}>
// CHECK-NEXT:    %29 = "llvm.insertvalue"(%1, %28) <{position = array<i32: 4, 1>}>
// CHECK-NEXT:    "llvm.br"(%4, %5)[^bb0] : (index, f32) -> ()
// CHECK-NEXT: ^bb0(%30: index, %31: f32):
// CHECK-NEXT:    %32 = "llvm.icmp"(%30, %2) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%32, %30, %31, %31)[^bb1, ^bb2]
// CHECK-NEXT: ^bb1(%33: index, %34: f32):
// CHECK-NEXT:    "llvm.br"(%33, %4, %34)[^bb3] : (index, index, f32) -> ()
// CHECK-NEXT: ^bb3(%35: index, %36: index, %37: f32):
// CHECK-NEXT:    %38 = "llvm.icmp"(%36, %3) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%38, %35, %36, %37, %35, %37)[^bb4, ^bb5]
// CHECK-NEXT: ^bb4(%39: index, %40: index, %41: f32):
// CHECK-NEXT:    %42 = "llvm.extractvalue"(%29) <{position = array<i32: 1>}>
// CHECK-NEXT:    %43 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 0>}>
// CHECK-NEXT:    %44 = "llvm.mul"(%39, %43) : (index, index) -> index
// CHECK-NEXT:    %45 = "llvm.extractvalue"(%29) <{position = array<i32: 4, 1>}>
// CHECK-NEXT:    %46 = "llvm.mul"(%40, %45) : (index, index) -> index
// CHECK-NEXT:    %47 = "llvm.add"(%44, %46) : (index, index) -> index
// CHECK-NEXT:    %48 = "llvm.getelementptr"(%42, %47) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %49 = llvm.load %48 : !llvm.ptr -> f32
// CHECK-NEXT:    %50 = "llvm.fadd"(%41, %49) : (f32, f32) -> f32
// CHECK-NEXT:    %51 = "llvm.add"(%40, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%39, %51, %50)[^bb3] : (index, index, f32) -> ()
// CHECK-NEXT: ^bb5(%52: index, %53: f32):
// CHECK-NEXT:    %54 = "llvm.add"(%52, %7) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%54, %53)[^bb0] : (index, f32) -> ()
// CHECK-NEXT: ^bb2(%55: f32):
// CHECK-NEXT:    %56 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}>
// CHECK-NEXT:    "llvm.call"(%56) <{callee = @free}> : (!llvm.ptr) -> ()
// CHECK-NEXT:    "llvm.return"(%55) : (f32) -> ()
// CHECK-NEXT: }

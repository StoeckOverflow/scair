// RUN: scair-opt %s -p lower-refined-dmemref-to-llvm | filecheck %s -dump-input=always
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
    
    %buf = d_memref.reinterpret_cast %flat to
      offset: [%zero],
      sizes: [%c256, %c1024],
      strides: [%stride0, %stride1]
    : !d_memref.memref<[%flat_nat], f32> to !d_memref.memref<[%d0, %d1], f32, offset: 0, strides: [%stride0, %stride1]>
    
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    %result = d_affine.for %i = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c256) step 1 : i32 iter_args(%acc = %cst : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c1024) step 1 : i32 iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %buf[%i, %j] : !d_memref.memref<[%d0, %d1], f32, offset: 0, strides: [%stride0, %stride1]> -> f32
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
// CHECK:         %2 = llvm.mlir.constant 1024 : index : index
// CHECK-NEXT:    %3 = llvm.mlir.constant 1 : index : index
// CHECK-NEXT:    %4 = llvm.mlir.constant 0 : index : index
// CHECK-NEXT:    %5 = llvm.mlir.constant 0.0 : f32 : f32
// CHECK-NEXT:    %6 = llvm.mlir.constant 256 : index : index
// CHECK-NEXT:    %7 = "llvm.mul"(%6, %0) : (index, index) -> index
// CHECK-NEXT:    %8 = llvm.mlir.zero : !llvm.ptr
// CHECK-NEXT:    %9 = "llvm.getelementptr"(%8, %7) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %10 = "llvm.ptrtoint"(%9) : (!llvm.ptr) -> index
// CHECK-NEXT:    %11 = "llvm.call"(%10) <{callee = @malloc}> : (index) -> !llvm.ptr
// CHECK-NEXT:    %12 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<1 x index>, !llvm.array<1 x index>)>
// CHECK-NEXT:    %13 = "llvm.insertvalue"(%11, %12) <{position = array<i32: 0>}>
// CHECK-NEXT:    %14 = "llvm.insertvalue"(%11, %13) <{position = array<i32: 1>}>
// CHECK-NEXT:    %15 = "llvm.insertvalue"(%4, %14) <{position = array<i32: 2>}>
// CHECK-NEXT:    %16 = "llvm.insertvalue"(%7, %15) <{position = array<i32: 3, 0>}>
// CHECK-NEXT:    %17 = "llvm.insertvalue"(%3, %16) <{position = array<i32: 4, 0>}>
// CHECK-NEXT:    %18 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}>
// CHECK-NEXT:    %19 = "llvm.extractvalue"(%17) <{position = array<i32: 1>}>
// CHECK-NEXT:    %20 = llvm.mlir.poison : !llvm.struct<(!llvm.ptr, !llvm.ptr, index, !llvm.array<2 x index>, !llvm.array<2 x index>)>
// CHECK-NEXT:    %21 = "llvm.insertvalue"(%18, %20) <{position = array<i32: 0>}>
// CHECK-NEXT:    %22 = "llvm.insertvalue"(%19, %21) <{position = array<i32: 1>}>
// CHECK-NEXT:    %23 = "llvm.insertvalue"(%4, %22) <{position = array<i32: 2>}>
// CHECK-NEXT:    %24 = "llvm.insertvalue"(%6, %23) <{position = array<i32: 3, 0>}>
// CHECK-NEXT:    %25 = "llvm.insertvalue"(%2, %24) <{position = array<i32: 3, 1>}>
// CHECK-NEXT:    %26 = "llvm.insertvalue"(%0, %25) <{position = array<i32: 4, 0>}>
// CHECK-NEXT:    %27 = "llvm.insertvalue"(%1, %26) <{position = array<i32: 4, 1>}>
// CHECK-NEXT:    "llvm.br"(%4, %5)[^bb0] : (index, f32) -> ()
// CHECK-NOT:     llvm.mlir.constant 1 : index : index
// CHECK-NEXT: ^bb0(%28: index, %29: f32):
// CHECK-NEXT:    %30 = "llvm.icmp"(%28, %6) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%30, %28, %29, %29)[^bb1, ^bb2]
// CHECK-NEXT: ^bb1(%31: index, %32: f32):
// CHECK-NEXT:    "llvm.br"(%31, %4, %32)[^bb3] : (index, index, f32) -> ()
// CHECK-NEXT: ^bb3(%33: index, %34: index, %35: f32):
// CHECK-NEXT:    %36 = "llvm.icmp"(%34, %2) <{predicate = "slt"}> : (index, index) -> i1
// CHECK-NEXT:    "llvm.cond_br"(%36, %33, %34, %35, %33, %35)[^bb4, ^bb5]
// CHECK-NEXT: ^bb4(%37: index, %38: index, %39: f32):
// CHECK-NEXT:    %40 = "llvm.extractvalue"(%27) <{position = array<i32: 1>}>
// CHECK-NEXT:    %41 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 0>}>
// CHECK-NEXT:    %42 = "llvm.mul"(%37, %41) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    %43 = "llvm.extractvalue"(%27) <{position = array<i32: 4, 1>}>
// CHECK-NEXT:    %44 = "llvm.mul"(%38, %43) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    %45 = "llvm.add"(%42, %44) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    %46 = "llvm.getelementptr"(%40, %45) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32, gepFlags = ["inbounds", "nuw"]}> : (!llvm.ptr, index) -> !llvm.ptr
// CHECK-NEXT:    %47 = llvm.load %46 : !llvm.ptr -> f32
// CHECK-NEXT:    %48 = "llvm.fadd"(%39, %47) : (f32, f32) -> f32
// CHECK-NEXT:    %49 = "llvm.add"(%38, %3) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%37, %49, %48)[^bb3] : (index, index, f32) -> ()
// CHECK-NEXT: ^bb5(%50: index, %51: f32):
// CHECK-NEXT:    %52 = "llvm.add"(%50, %3) <{overflowFlags = ["nsw", "nuw"]}> : (index, index) -> index
// CHECK-NEXT:    "llvm.br"(%52, %51)[^bb0] : (index, f32) -> ()
// CHECK-NEXT: ^bb2(%53: f32):
// CHECK-NEXT:    %54 = "llvm.extractvalue"(%17) <{position = array<i32: 0>}>
// CHECK-NEXT:    "llvm.call"(%54) <{callee = @free}> : (!llvm.ptr) -> ()
// CHECK-NEXT:    "llvm.return"(%53) : (f32) -> ()
// CHECK-NEXT: }

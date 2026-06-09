// RUN: scair-opt %s -p lower-d-memref-to-llvm | filecheck %s --implicit-check-not=llvm.extractvalue --implicit-check-not=llvm.insertvalue -dump-input=always

builtin.module {
  func.func @semi_affine_layout_map(%stride0 : index, %stride1 : index) -> f32 {
    %c256 = "arith.constant"() <{value = 256 : index}> : () -> index
    %c1024 = "arith.constant"() <{value = 1024 : index}> : () -> index
    %zero = "arith.constant"() <{value = 0 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %f1 = "arith.constant"() <{value = 1.0 : f32}> : () -> f32

    %total = "arith.muli"(%c256, %stride0) : (index, index) -> index
    %flat_size = "d_tensor.size.import"(%total) : (index) -> !d_tensor.size
    %flat = d_memref.alloc : () -> !d_memref.memref<[%flat_size], f32>

    %d0 = "d_tensor.size.constant"() <{value = 256 : i32}> : () -> !d_tensor.size
    %d1 = "d_tensor.size.constant"() <{value = 1024 : i32}> : () -> !d_tensor.size

    %buf = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%flat_size], f32> to !d_memref.memref<[%d0, %d1], f32, offset: %zero, strides: [%stride0, %stride1]>

    d_affine.for %i = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c256) step 1 : index {
      d_affine.for %j = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index {
        d_memref.store %f1, %buf[%i, %j] : f32, !d_memref.memref<[%d0, %d1], f32, offset: %zero, strides: [%stride0, %stride1]>
        d_affine.yield
      }
      d_affine.yield
    }

    %result = d_affine.for %i = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c256) step 1 : index iter_args(%acc = %f0 : f32) {
      %inner = d_affine.for %j = affine_map<(d0) -> (d0)>(%zero) to affine_map<(d0) -> (d0)>(%c1024) step 1 : index iter_args(%acc2 = %acc : f32) {
        %v = d_memref.load %buf[%i, %j] : !d_memref.memref<[%d0, %d1], f32, offset: %zero, strides: [%stride0, %stride1]> -> f32
        %sum = "arith.addf"(%acc2, %v) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
        d_affine.yield %sum : (f32)
      }
      d_affine.yield %inner : (f32)
    }
    d_memref.dealloc %flat : !d_memref.memref<[%flat_size], f32>
    func.return %result : f32
  }
}

// CHECK-LABEL: func.func @semi_affine_layout_map(%0: i64, %1: i64) -> f32 {
// CHECK: %2 = "llvm.mlir.constant"() <{value = 0}> : () -> i64
// CHECK: %11 = "llvm.mul"(%6, %3) : (i64, i64) -> i64
// CHECK: %13 = "llvm.getelementptr"(%12, %11) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %14 = "llvm.ptrtoint"(%13) : (!llvm.ptr) -> i64
// CHECK: %17 = llvm.icmp "slt" %16, %6 : i64
// CHECK: %24 = "llvm.getelementptr"(%15, %23) <{rawConstantIndices = array<i32: -2147483648>, elem_type = f32}> : (!llvm.ptr, i64) -> !llvm.ptr
// CHECK: %39 = llvm.load %38 : !llvm.ptr -> f32
// CHECK: llvm.call @free(%15) : (!llvm.ptr) -> ()
// CHECK: "llvm.return"(%28) : (f32) -> ()

builtin.module {
  func.func @semi_affine_fill_and_sum(
      %stride0 : index,
      %stride1 : index,
      %flat : memref<?xf32>,
      %out : memref<1xf32>) attributes {llvm.emit_c_interface} {
    %c256 = arith.constant 256 : index
    %c1024 = arith.constant 1024 : index
    %c0 = arith.constant 0 : index
    %f0 = arith.constant 0.0 : f32
    %f1 = arith.constant 1.0 : f32

    %buf = memref.reinterpret_cast %flat to
      offset: [0],
      sizes: [256, 1024],
      strides: [%stride0, %stride1]
    : memref<?xf32> to memref<256x1024xf32, strided<[?, ?], offset: 0>>

    affine.for %i = 0 to 256 {
      affine.for %j = 0 to 1024 {
        affine.store %f1, %buf[%i, %j] : memref<256x1024xf32, strided<[?, ?], offset: 0>>
      }
    }

    %sum = affine.for %i = 0 to 256 iter_args(%acc = %f0) -> (f32) {
      %inner = affine.for %j = 0 to 1024 iter_args(%acc2 = %acc) -> (f32) {
        %v = affine.load %buf[%i, %j] : memref<256x1024xf32, strided<[?, ?], offset: 0>>
        %next = arith.addf %acc2, %v : f32
        affine.yield %next : f32
      }
      affine.yield %inner : f32
    }

    memref.store %sum, %out[%c0] : memref<1xf32>
    return
  }
}

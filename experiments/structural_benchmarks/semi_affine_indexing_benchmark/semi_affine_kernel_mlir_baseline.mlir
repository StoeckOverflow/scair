builtin.module {
  func.func @semi_affine_fill_and_sum(
      %rows : index,
      %cols : index,
      %stride0 : index,
      %stride1 : index,
      %flat : memref<?xf32>,
      %out : memref<1xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %f0 = arith.constant 0.0 : f32
    %f1 = arith.constant 1.0 : f32

    %buf = memref.reinterpret_cast %flat to
      offset: [0],
      sizes: [%rows, %cols],
      strides: [%stride0, %stride1]
    : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: 0>>

    affine.for %i = 0 to affine_map<(d0) -> (d0)>(%rows) {
      affine.for %j = 0 to affine_map<(d0) -> (d0)>(%cols) {
        affine.store %f1, %buf[%i, %j] : memref<?x?xf32, strided<[?, ?], offset: 0>>
      }
    }

    %sum = affine.for %i = 0 to affine_map<(d0) -> (d0)>(%rows) iter_args(%acc = %f0) -> (f32) {
      %inner = affine.for %j = 0 to affine_map<(d0) -> (d0)>(%cols) iter_args(%acc2 = %acc) -> (f32) {
        %v = affine.load %buf[%i, %j] : memref<?x?xf32, strided<[?, ?], offset: 0>>
        %next = arith.addf %acc2, %v : f32
        affine.yield %next : f32
      }
      affine.yield %inner : f32
    }

    memref.store %sum, %out[%c0] : memref<1xf32>
    return
  }
}

func.func @strided_fill_and_sum(
    %stride0 : index,
    %stride1 : index,
    %flat : memref<?xf32>,
    %out : memref<1xf32>) {

  %buf = memref.reinterpret_cast %flat to
    offset: [0], sizes: [256, 1024], strides: [%stride0, %stride1]
    : memref<?xf32> to memref<256x1024xf32, strided<[?, ?], offset: 0>>

  affine.for %i = 0 to 256 {
    affine.for %j = 0 to 1024 {
      memref.store %f1, %buf[%i, %j]
        : memref<256x1024xf32, strided<[?, ?], offset: 0>>
    }
  }

  %v = memref.load %buf[%i, %j]: memref<256x1024xf32, strided<[?, ?], offset: 0>
}
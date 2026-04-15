func.func @strided_fill_and_sum(
  %stride0 : index,
  %stride1 : index,
  %buf : !d_memref.memref<[256, 1024], f32,
         offset: 0, strides: [%stride0, %stride1]>,
  %out : !d_memref.memref<[1], f32>) {

  d_affine.for %i = ... {
    d_affine.for %j = ... {
      d_memref.store %f1, %buf[%i, %j]
    }
  }

  %v = d_memref.load %buf[%i, %j]
    : !d_memref.memref<[256, 1024], f32,
      offset: 0, strides: [%stride0, %stride1]> -> f32
}
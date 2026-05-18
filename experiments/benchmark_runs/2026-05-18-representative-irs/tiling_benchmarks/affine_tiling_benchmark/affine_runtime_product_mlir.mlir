builtin.module {
  func.func @affine_runtime_product(%k0: index, %k1: index, %out: memref<?xf32>) {
    %k = arith.muli %k0, %k1 : index
    %cst = arith.constant 0.0 : f32
    affine.for %p = 0 to %k {
      memref.store %cst, %out[%p] : memref<?xf32>
    }
    return
  }
}

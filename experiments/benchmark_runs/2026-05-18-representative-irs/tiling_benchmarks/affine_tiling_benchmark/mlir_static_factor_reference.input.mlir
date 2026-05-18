#map = affine_map<()[s0] -> (s0 * 3)>

builtin.module {
  func.func @affine_static_factor(%k0: index, %out: memref<?xf32>) {
    %cst = arith.constant 0.0 : f32
    affine.for %p = 0 to #map()[%k0] {
      memref.store %cst, %out[%p] : memref<?xf32>
    }
    return
  }
}

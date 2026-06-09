builtin.module {
  func.func @dependent_exact_dynamic(%k0: !d_tensor.size, %k1: !d_tensor.pos_size, %out: memref<?xf32>) {
    %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

builtin.module {
  func.func @nested_commuted_product(%out: memref<?xf32>) {
    %k0 = "d_tensor.size.constant"() <{value = 5 : i32}> : () -> !d_tensor.size
    %k1 = "d_tensor.size.constant"() <{value = 3 : i32}> : () -> !d_tensor.size
    %k2 = "d_tensor.size.constant"() <{value = 7 : i32}> : () -> !d_tensor.size
    %k10 = "d_tensor.size.mul"(%k1, %k0) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %k = "d_tensor.size.mul"(%k10, %k2) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

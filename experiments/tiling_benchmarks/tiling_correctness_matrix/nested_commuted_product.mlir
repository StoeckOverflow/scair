builtin.module {
  func.func @nested_commuted_product(%out: memref<?xf32>) {
    %k0 = "d_tensor.nat.const"() <{value = 5 : i32}> : () -> !d_tensor.nat
    %k1 = "d_tensor.nat.const"() <{value = 3 : i32}> : () -> !d_tensor.nat
    %k2 = "d_tensor.nat.const"() <{value = 7 : i32}> : () -> !d_tensor.nat
    %k10 = "d_tensor.nat.mul"(%k1, %k0) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %k = "d_tensor.nat.mul"(%k10, %k2) : (!d_tensor.nat, !d_tensor.nat) -> !d_tensor.nat
    %ub = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

builtin.module {
  func.func @affine_value_dependent_product(
    %k0_nat: !d_tensor.nat,
    %k1_nat: !d_tensor.posnat,
    %out: memref<?xf32>
  ) {
    %k_nat = "d_tensor.nat.mul"(%k0_nat, %k1_nat) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
    %k = "d_tensor.shape.to_index"(%k_nat) : (!d_tensor.nat) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %cst = "arith.constant"() <{value = 0.0 : f32}> : () -> f32

    d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index {
      "memref.store"(%cst, %out, %p) : (f32, memref<?xf32>, index) -> ()
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}

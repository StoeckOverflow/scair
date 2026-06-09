builtin.module {
  %k0 = "d_tensor.nat.param"() : () -> !d_tensor.nat
  %k1 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
  %k = "d_tensor.nat.mul"(%k0, %k1) : (!d_tensor.nat, !d_tensor.posnat) -> !d_tensor.nat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "d_tensor.shape.to_index"(%k) : (!d_tensor.nat) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

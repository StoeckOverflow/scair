builtin.module {
  %k0 = "d_tensor.size.param"() : () -> !d_tensor.size
  %k1 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
  %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.pos_size) -> !d_tensor.size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

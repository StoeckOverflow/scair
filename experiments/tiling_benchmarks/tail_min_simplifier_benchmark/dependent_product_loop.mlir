builtin.module {
  %k0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %k1 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %k = "dtensor.nat.mul"(%k0, %k1) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %ub = "dtensor.shape.to_index"(%k) : (!dtensor.nat) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%ub) step 1 : index iter_args(%acc = %init : index) {
    %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
    d_affine.yield %next : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

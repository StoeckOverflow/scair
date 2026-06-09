builtin.module {
  func.func @runtime_checked_dynamic(%k0_idx: index, %k1_idx: index) -> index {
    %k0 = "d_tensor.size.import"(%k0_idx) : (index) -> !d_tensor.size
    %k1 = "d_tensor.size.import"(%k1_idx) : (index) -> !d_tensor.size
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%k1, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
    %k = "d_tensor.size.mul"(%k0, %k1) : (!d_tensor.size, !d_tensor.size) -> !d_tensor.size

    %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %c0 : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc] : (index)[index] -> index
      d_affine.yield %next : (index)
    }

    "func.return"(%sum) : (index) -> ()
  }
}

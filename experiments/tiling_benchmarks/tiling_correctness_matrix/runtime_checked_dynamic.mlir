builtin.module {
  func.func @runtime_checked_dynamic(%k0: index, %k1: index) -> index {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %ok = "arith.cmpi"(%k1, %c0) <{predicate = 4 : i64}> : (index, index) -> i1
    "cf.assert"(%ok) <{msg = "k1 must be positive"}> : (i1) -> ()
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index

    %sum = d_affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %c0 : index) {
      %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc)[] : (index, index)[] -> index
      d_affine.yield %next : (index)
    }

    "func.return"(%sum) : (index) -> ()
  }
}

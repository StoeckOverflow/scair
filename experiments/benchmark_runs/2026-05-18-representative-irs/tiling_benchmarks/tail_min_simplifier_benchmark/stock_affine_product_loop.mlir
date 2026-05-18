builtin.module {
  func.func @stock_affine_product_loop(%k0 : index, %k1 : index) -> index {
    %k = "arith.muli"(%k0, %k1) : (index, index) -> index
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %init = "arith.constant"() <{value = 0 : index}> : () -> index

    %sum = affine.for %p = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%k) step 1 : index iter_args(%acc = %init : index) {
      %next = "arith.addi"(%p, %acc) : (index, index) -> index
      affine.yield %next : index
    }
    func.return %sum : index
  }
}

builtin.module {
  %k0 = "test.arg"() : () -> index
  %k1 = "test.arg"() : () -> index
  %k2 = "test.arg"() : () -> index
  %tile = "arith.muli"(%k1, %k0) : (index, index) -> index
  %full = "arith.muli"(%tile, %k2) : (index, index) -> index
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %tile_iv = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %tile : index iter_args(%acc0 = %init : index) {
    %tile_end = "arith.addi"(%tile_iv, %tile) : (index, index) -> index
    %clamped = "arith.minsi"(%tile_end, %full) : (index, index) -> index
    %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile_iv) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
      %next = d_affine.apply affine_map<(d0, d1) -> (d0 + d1)>(%p, %acc1)[] : (index, index)[] -> index
      d_affine.yield %next : (index)
    }
    d_affine.yield %inner : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

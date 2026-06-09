builtin.module {
  %k0 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
  %k1 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
  %k2 = "d_tensor.size.param"() : () -> !d_tensor.pos_size
  %tile_size = "d_tensor.size.mul"(%k1, %k0) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
  %full_size = "d_tensor.size.mul"(%tile_size, %k2) : (!d_tensor.pos_size, !d_tensor.pos_size) -> !d_tensor.pos_size
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full_size) step %tile_size : index iter_args(%acc0 = %init : index) {
    %tile_end = "arith.addi"(%tile, %tile_size) : (index, index) -> index
    %clamped = "arith.minsi"(%tile_end, %full_size) : (index, index) -> index
    %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc1] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    d_affine.yield %inner : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

builtin.module {
  %k0 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
  %k1 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
  %k2 = "d_tensor.nat.param"() : () -> !d_tensor.posnat
  %tile_nat = "d_tensor.nat.mul"(%k1, %k0) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
  %full_nat = "d_tensor.nat.mul"(%tile_nat, %k2) : (!d_tensor.posnat, !d_tensor.posnat) -> !d_tensor.posnat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %full = "d_tensor.shape.to_index"(%full_nat) : (!d_tensor.posnat) -> index
  %step = "d_tensor.shape.to_index"(%tile_nat) : (!d_tensor.posnat) -> index
  %tile_size = "d_tensor.shape.to_index"(%tile_nat) : (!d_tensor.posnat) -> index
  %init = "arith.constant"() <{value = 0 : index}> : () -> index

  %sum = d_affine.for %tile = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%full) step %step : index iter_args(%acc0 = %init : index) {
    %tile_end = "arith.addi"(%tile, %tile_size) : (index, index) -> index
    %clamped = "arith.minsi"(%tile_end, %full) : (index, index) -> index
    %inner = d_affine.for %p = affine_map<(d0) -> (d0)>(%tile) to affine_map<(d0) -> (d0)>(%clamped) step 1 : index iter_args(%acc1 = %acc0 : index) {
      %next = d_affine.apply affine_map<(d0)[s0] -> (d0 + s0)>(%p)[%acc1] : (index)[index] -> index
      d_affine.yield %next : (index)
    }
    d_affine.yield %inner : (index)
  }
  "test.keep"(%sum) : (index) -> ()
}

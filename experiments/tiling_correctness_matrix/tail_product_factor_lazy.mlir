builtin.module {
  %k0 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %k1 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %k2 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %tile_nat = "dtensor.nat.mul"(%k1, %k0) : (!dtensor.posnat, !dtensor.posnat) -> !dtensor.posnat
  %full_nat = "dtensor.nat.mul"(%tile_nat, %k2) : (!dtensor.posnat, !dtensor.posnat) -> !dtensor.posnat
  %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
  %full = "dtensor.shape.to_index"(%full_nat) : (!dtensor.posnat) -> index
  %step = "dtensor.shape.to_index"(%tile_nat) : (!dtensor.posnat) -> index
  %tile_size = "dtensor.shape.to_index"(%tile_nat) : (!dtensor.posnat) -> index
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

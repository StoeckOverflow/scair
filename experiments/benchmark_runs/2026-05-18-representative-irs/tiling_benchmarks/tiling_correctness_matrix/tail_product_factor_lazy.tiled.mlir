#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + s0)>
builtin.module {
  %0 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %1 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %2 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %3 = "dtensor.nat.mul"(%1, %0) : (!dtensor.posnat, !dtensor.posnat) -> !dtensor.posnat
  %4 = "dtensor.nat.mul"(%3, %2) : (!dtensor.posnat, !dtensor.posnat) -> !dtensor.posnat
  %5 = "arith.constant"() <{value = 0 : index}> : () -> index
  %6 = "dtensor.shape.to_index"(%4) : (!dtensor.posnat) -> index
  %7 = "dtensor.shape.to_index"(%3) : (!dtensor.posnat) -> index
  %8 = "dtensor.shape.to_index"(%3) : (!dtensor.posnat) -> index
  %9 = "arith.constant"() <{value = 0 : index}> : () -> index
  %10 = d_affine.for %11 = #map(%5) to #map(%6) step %7 : index iter_args(%12 = %9 : index) {
    %13 = "arith.addi"(%11, %8) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %14 = d_affine.for %15 = #map(%11) to #map(%13) step 1 : index iter_args(%16 = %12 : index) {
      %17 = d_affine.apply #map1 (%15)[%16] : (index)[index] -> index
      d_affine.yield %17 : (index)
    }
    d_affine.yield %14 : (index)
  }
  "test.keep"(%10) : (index) -> ()
}

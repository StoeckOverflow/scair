#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + s0)>
builtin.module {
  %0 = "dtensor.nat.param"() : () -> !dtensor.nat
  %1 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %2 = "dtensor.nat.mul"(%0, %1) : (!dtensor.nat, !dtensor.posnat) -> !dtensor.nat
  %3 = "dtensor.shape.to_index"(%2) : (!dtensor.nat) -> index
  %4 = "arith.constant"() <{value = 0 : index}> : () -> index
  %5 = "dtensor.shape.to_index"(%1) : (!dtensor.posnat) -> index
  %6 = d_affine.for %7 = #map(%4) to #map(%3) step %5 : index iter_args(%8 = %4 : index) {
    %9 = "arith.addi"(%7, %5) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %10 = d_affine.for %11 = #map(%7) to #map(%9) step 1 : i32 iter_args(%12 = %8 : index) {
      %13 = d_affine.apply #map1 (%11)[%12] : (index)[index] -> index
      d_affine.yield %13 : (index)
    }
    d_affine.yield %10 : (index)
  }
  "test.keep"(%6) : (index) -> ()
}

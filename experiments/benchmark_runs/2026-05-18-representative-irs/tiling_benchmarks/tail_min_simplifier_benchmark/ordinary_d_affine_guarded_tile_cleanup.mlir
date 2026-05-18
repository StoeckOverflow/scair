#map = affine_map<(d0)[] -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + s0)>
builtin.module {
  %0 = "test.arg"() : () -> index
  %1 = "dtensor.nat.param"() : () -> !dtensor.posnat
  %2 = "dtensor.shape.to_index"(%1) : (!dtensor.posnat) -> index
  %3 = "arith.muli"(%0, %2) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
  %4 = "arith.constant"() <{value = 0 : index}> : () -> index
  %5 = d_affine.for %6 = #map(%4) to #map(%3) step %2 : index iter_args(%7 = %4 : index) {
    %8 = "arith.addi"(%6, %2) <{overflowFlags = #arith.overflow<none>}> : (index, index) -> index
    %9 = "arith.minsi"(%8, %3) : (index, index) -> index
    %10 = d_affine.for %11 = #map(%6) to #map(%9) step 1 : i32 iter_args(%12 = %7 : index) {
      %13 = d_affine.apply #map1 (%11)[%12] : (index)[index] -> index
      d_affine.yield %13 : (index)
    }
    d_affine.yield %10 : (index)
  }
  "test.keep"(%5) : (index) -> ()
}

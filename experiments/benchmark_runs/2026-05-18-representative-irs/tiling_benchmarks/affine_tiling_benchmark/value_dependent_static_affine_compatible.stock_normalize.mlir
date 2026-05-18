#map = affine_map<()[s0] -> (s0 ceildiv 3)>
#map1 = affine_map<(d0) -> (d0 * 3)>
#map2 = affine_map<(d0, d1) -> (d0 + d1)>
module {
  func.func @affine_value_dependent_static_product(%arg0: !dtensor.nat, %arg1: memref<?xf32>) {
    %0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %1 = "dtensor.nat.mul"(%0, %arg0) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %2 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
    %cst = arith.constant 0.000000e+00 : f32
    %c0 = arith.constant 0 : index
    affine.for %arg2 = 0 to #map()[%2] {
      %3 = affine.apply #map1(%arg2)
      affine.for %arg3 = 0 to 3 {
        %4 = affine.apply #map2(%3, %arg3)
        memref.store %cst, %arg1[%4] : memref<?xf32>
      }
    }
    return
  }
}


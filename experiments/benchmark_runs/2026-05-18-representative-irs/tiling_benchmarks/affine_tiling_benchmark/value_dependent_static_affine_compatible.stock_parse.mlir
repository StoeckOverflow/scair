#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0) -> (d0 + 3)>
module {
  func.func @affine_value_dependent_static_product(%arg0: !dtensor.nat, %arg1: memref<?xf32>) {
    %0 = "dtensor.nat.const"() <{value = 3 : i32}> : () -> !dtensor.nat
    %1 = "dtensor.nat.mul"(%0, %arg0) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat
    %2 = "dtensor.shape.to_index"(%1) : (!dtensor.nat) -> index
    %cst = arith.constant 0.000000e+00 : f32
    %c0 = arith.constant 0 : index
    affine.for %arg2 = #map(%c0) to #map(%2) step 3 {
      affine.for %arg3 = #map(%arg2) to #map1(%arg2) {
        memref.store %cst, %arg1[%arg3] : memref<?xf32>
      }
    }
    return
  }
}


#map = affine_map<()[s0] -> (s0 * 3)>
#map1 = affine_map<(d0) -> (d0)>
#map2 = affine_map<(d0) -> (d0 + 3)>
module {
  func.func @affine_static_factor(%arg0: index, %arg1: memref<?xf32>) {
    %cst = arith.constant 0.000000e+00 : f32
    affine.for %arg2 = 0 to #map()[%arg0] step 3 {
      affine.for %arg3 = #map1(%arg2) to #map2(%arg2) {
        memref.store %cst, %arg1[%arg3] : memref<?xf32>
      }
    }
    return
  }
}


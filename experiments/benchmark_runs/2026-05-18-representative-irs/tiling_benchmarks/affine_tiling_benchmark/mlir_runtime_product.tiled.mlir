#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 3, s0)>
module {
  func.func @affine_runtime_product(%arg0: index, %arg1: index, %arg2: memref<?xf32>) {
    %0 = arith.muli %arg0, %arg1 : index
    %cst = arith.constant 0.000000e+00 : f32
    affine.for %arg3 = 0 to %0 step 3 {
      affine.for %arg4 = #map(%arg3) to min #map1(%arg3)[%0] {
        memref.store %cst, %arg2[%arg4] : memref<?xf32>
      }
    }
    return
  }
}


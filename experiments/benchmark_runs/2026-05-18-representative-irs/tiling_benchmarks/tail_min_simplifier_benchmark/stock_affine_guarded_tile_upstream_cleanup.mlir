#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 4, s0)>
module {
  func.func @stock_affine_product_loop(%arg0: index, %arg1: index) -> index {
    %c0 = arith.constant 0 : index
    %0 = arith.muli %arg0, %arg1 : index
    %1 = affine.for %arg2 = 0 to %0 step 4 iter_args(%arg3 = %c0) -> (index) {
      %2 = affine.for %arg4 = #map(%arg2) to min #map1(%arg2)[%0] iter_args(%arg5 = %arg3) -> (index) {
        %3 = arith.addi %arg4, %arg5 : index
        affine.yield %3 : index
      }
      affine.yield %2 : index
    }
    return %1 : index
  }
}


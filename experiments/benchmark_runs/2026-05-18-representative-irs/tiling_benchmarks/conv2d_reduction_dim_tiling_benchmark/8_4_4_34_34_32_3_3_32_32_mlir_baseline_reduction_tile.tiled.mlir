#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 36, s0)>
module {
  func.func @conv2d_reduction_dim_tiling(%arg0: index, %arg1: index, %arg2: index, %arg3: index, %arg4: index, %arg5: index, %arg6: index, %arg7: index, %arg8: index, %arg9: index, %arg10: memref<?xf32>, %arg11: memref<?xf32>, %arg12: memref<?xf32>) {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %cst = arith.constant 0.000000e+00 : f32
    %0 = arith.muli %arg1, %arg2 : index
    %1 = arith.muli %arg3, %arg4 : index
    %2 = arith.muli %0, %1 : index
    %3 = arith.muli %arg6, %arg7 : index
    %4 = arith.muli %arg2, %3 : index
    %5 = arith.muli %arg1, %4 : index
    %6 = arith.muli %arg8, %arg9 : index
    %7 = arith.muli %arg5, %6 : index
    %reinterpret_cast = memref.reinterpret_cast %arg10 to offset: [%c0], sizes: [%arg0, %0, %arg8, %arg9, %arg6, %arg7], strides: [%2, %1, %arg4, %c1, %arg4, %c1] : memref<?xf32> to memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
    %reinterpret_cast_0 = memref.reinterpret_cast %arg11 to offset: [%c0], sizes: [%arg5, %0, %arg6, %arg7], strides: [%5, %3, %arg7, %c1] : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %reinterpret_cast_1 = memref.reinterpret_cast %arg12 to offset: [%c0], sizes: [%arg0, %arg5, %arg8, %arg9], strides: [%7, %6, %arg9, %c1] : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    affine.for %arg13 = 0 to %arg0 step 36 {
      affine.for %arg14 = 0 to %arg5 step 36 {
        affine.for %arg15 = 0 to %arg8 step 36 {
          affine.for %arg16 = 0 to %arg9 step 36 {
            affine.for %arg17 = #map(%arg13) to min #map1(%arg13)[%arg0] {
              affine.for %arg18 = #map(%arg14) to min #map1(%arg14)[%arg5] {
                affine.for %arg19 = #map(%arg15) to min #map1(%arg15)[%arg8] {
                  affine.for %arg20 = #map(%arg16) to min #map1(%arg16)[%arg9] {
                    %8 = affine.for %arg21 = 0 to %5 iter_args(%arg22 = %cst) -> (f32) {
                      %9 = arith.divui %arg21, %3 : index
                      %10 = arith.remui %arg21, %3 : index
                      %11 = arith.divui %10, %arg7 : index
                      %12 = arith.remui %10, %arg7 : index
                      %13 = memref.load %reinterpret_cast[%arg17, %9, %arg19, %arg20, %11, %12] : memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
                      %14 = memref.load %reinterpret_cast_0[%arg18, %9, %11, %12] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                      %15 = arith.mulf %13, %14 : f32
                      %16 = arith.addf %arg22, %15 : f32
                      affine.yield %16 : f32
                    }
                    memref.store %8, %reinterpret_cast_1[%arg17, %arg18, %arg19, %arg20] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                  }
                }
              }
            }
          }
        }
      }
    }
    return
  }
}


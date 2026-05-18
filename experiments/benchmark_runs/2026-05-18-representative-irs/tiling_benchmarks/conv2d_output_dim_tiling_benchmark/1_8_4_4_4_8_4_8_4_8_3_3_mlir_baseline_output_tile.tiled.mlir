#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 8, s0)>
module {
  func.func @conv2d_output_dim_tiling(%arg0: index, %arg1: index, %arg2: index, %arg3: index, %arg4: index, %arg5: index, %arg6: index, %arg7: index, %arg8: index, %arg9: index, %arg10: index, %arg11: index, %arg12: index, %arg13: memref<?xf32>, %arg14: memref<?xf32>, %arg15: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %cst = arith.constant 0.000000e+00 : f32
    %0 = arith.muli %arg0, %arg1 : index
    %1 = arith.muli %arg5, %arg6 : index
    %2 = arith.muli %arg9, %arg10 : index
    %3 = arith.muli %arg11, %arg12 : index
    %4 = arith.muli %arg3, %arg4 : index
    %5 = arith.muli %arg2, %4 : index
    %6 = arith.muli %arg7, %arg8 : index
    %7 = arith.muli %arg2, %6 : index
    %8 = arith.muli %2, %3 : index
    %9 = arith.muli %1, %8 : index
    %reinterpret_cast = memref.reinterpret_cast %arg13 to offset: [%c0], sizes: [%0, %arg2, %2, %3, %arg7, %arg8], strides: [%5, %4, %arg4, %c1, %arg4, %c1] : memref<?xf32> to memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
    %reinterpret_cast_0 = memref.reinterpret_cast %arg14 to offset: [%c0], sizes: [%1, %arg2, %arg7, %arg8], strides: [%7, %6, %arg8, %c1] : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %reinterpret_cast_1 = memref.reinterpret_cast %arg15 to offset: [%c0], sizes: [%0, %1, %2, %3], strides: [%9, %8, %3, %c1] : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    affine.for %arg16 = 0 to %0 step 8 {
      affine.for %arg17 = 0 to %1 step 8 {
        affine.for %arg18 = 0 to %2 step 8 {
          affine.for %arg19 = 0 to %3 step 8 {
            affine.for %arg20 = #map(%arg16) to min #map1(%arg16)[%0] {
              affine.for %arg21 = #map(%arg17) to min #map1(%arg17)[%1] {
                affine.for %arg22 = #map(%arg18) to min #map1(%arg18)[%2] {
                  affine.for %arg23 = #map(%arg19) to min #map1(%arg19)[%3] {
                    %10 = affine.for %arg24 = 0 to %7 iter_args(%arg25 = %cst) -> (f32) {
                      %11 = arith.divui %arg24, %6 : index
                      %12 = arith.remui %arg24, %6 : index
                      %13 = arith.divui %12, %arg8 : index
                      %14 = arith.remui %12, %arg8 : index
                      %15 = memref.load %reinterpret_cast[%arg20, %11, %arg22, %arg23, %13, %14] : memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
                      %16 = memref.load %reinterpret_cast_0[%arg21, %11, %13, %14] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                      %17 = arith.mulf %15, %16 : f32
                      %18 = arith.addf %arg25, %17 : f32
                      affine.yield %18 : f32
                    }
                    memref.store %10, %reinterpret_cast_1[%arg20, %arg21, %arg22, %arg23] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
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


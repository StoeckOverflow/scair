#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 8, s0)>
module {
  func.func @conv2d_full_factorized_tiling(%arg0: index, %arg1: index, %arg2: index, %arg3: index, %arg4: index, %arg5: index, %arg6: index, %arg7: index, %arg8: index, %arg9: index, %arg10: index, %arg11: index, %arg12: index, %arg13: index, %arg14: memref<?xf32>, %arg15: memref<?xf32>, %arg16: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %cst = arith.constant 0.000000e+00 : f32
    %0 = arith.muli %arg0, %arg1 : index
    %1 = arith.muli %arg2, %arg3 : index
    %2 = arith.muli %arg6, %arg7 : index
    %3 = arith.muli %arg10, %arg11 : index
    %4 = arith.muli %arg12, %arg13 : index
    %5 = arith.muli %arg4, %arg5 : index
    %6 = arith.muli %1, %5 : index
    %7 = arith.muli %arg8, %arg9 : index
    %8 = arith.muli %arg3, %7 : index
    %9 = arith.muli %arg2, %8 : index
    %10 = arith.muli %3, %4 : index
    %11 = arith.muli %2, %10 : index
    %reinterpret_cast = memref.reinterpret_cast %arg14 to offset: [%c0], sizes: [%0, %1, %3, %4, %arg8, %arg9], strides: [%6, %5, %arg5, %c1, %arg5, %c1] : memref<?xf32> to memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
    %reinterpret_cast_0 = memref.reinterpret_cast %arg15 to offset: [%c0], sizes: [%2, %1, %arg8, %arg9], strides: [%9, %7, %arg9, %c1] : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    %reinterpret_cast_1 = memref.reinterpret_cast %arg16 to offset: [%c0], sizes: [%0, %2, %3, %4], strides: [%11, %10, %4, %c1] : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
    affine.for %arg17 = 0 to %0 step 8 {
      affine.for %arg18 = 0 to %2 step 8 {
        affine.for %arg19 = 0 to %3 step 8 {
          affine.for %arg20 = 0 to %4 step 8 {
            affine.for %arg21 = #map(%arg17) to min #map1(%arg17)[%0] {
              affine.for %arg22 = #map(%arg18) to min #map1(%arg18)[%2] {
                affine.for %arg23 = #map(%arg19) to min #map1(%arg19)[%3] {
                  affine.for %arg24 = #map(%arg20) to min #map1(%arg20)[%4] {
                    %12 = affine.for %arg25 = 0 to %9 iter_args(%arg26 = %cst) -> (f32) {
                      %13 = arith.divui %arg25, %7 : index
                      %14 = arith.remui %arg25, %7 : index
                      %15 = arith.divui %14, %arg9 : index
                      %16 = arith.remui %14, %arg9 : index
                      %17 = memref.load %reinterpret_cast[%arg21, %13, %arg23, %arg24, %15, %16] : memref<?x?x?x?x?x?xf32, strided<[?, ?, ?, ?, ?, ?], offset: ?>>
                      %18 = memref.load %reinterpret_cast_0[%arg22, %13, %15, %16] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
                      %19 = arith.mulf %17, %18 : f32
                      %20 = arith.addf %arg26, %19 : f32
                      affine.yield %20 : f32
                    }
                    memref.store %12, %reinterpret_cast_1[%arg21, %arg22, %arg23, %arg24] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
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


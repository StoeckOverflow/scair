#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 64, s0)>
module {
  func.func @matmul_full_factorized_tiling(%arg0: index, %arg1: index, %arg2: index, %arg3: index, %arg4: index, %arg5: index, %arg6: memref<?xf32>, %arg7: memref<?xf32>, %arg8: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %cst = arith.constant 0.000000e+00 : f32
    %0 = arith.muli %arg0, %arg1 : index
    %1 = arith.muli %arg2, %arg3 : index
    %2 = arith.muli %arg4, %arg5 : index
    %reinterpret_cast = memref.reinterpret_cast %arg6 to offset: [%c0], sizes: [%0, %2], strides: [%2, %c1] : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %reinterpret_cast_0 = memref.reinterpret_cast %arg7 to offset: [%c0], sizes: [%2, %1], strides: [%1, %c1] : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %reinterpret_cast_1 = memref.reinterpret_cast %arg8 to offset: [%c0], sizes: [%0, %1], strides: [%1, %c1] : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    affine.for %arg9 = 0 to %0 step 64 {
      affine.for %arg10 = 0 to %1 step 64 {
        affine.for %arg11 = #map(%arg9) to min #map1(%arg9)[%0] {
          affine.for %arg12 = #map(%arg10) to min #map1(%arg10)[%1] {
            %3 = affine.for %arg13 = 0 to %2 iter_args(%arg14 = %cst) -> (f32) {
              %4 = memref.load %reinterpret_cast[%arg11, %arg13] : memref<?x?xf32, strided<[?, ?], offset: ?>>
              %5 = memref.load %reinterpret_cast_0[%arg13, %arg12] : memref<?x?xf32, strided<[?, ?], offset: ?>>
              %6 = arith.mulf %4, %5 : f32
              %7 = arith.addf %arg14, %6 : f32
              affine.yield %7 : f32
            }
            memref.store %3, %reinterpret_cast_1[%arg11, %arg12] : memref<?x?xf32, strided<[?, ?], offset: ?>>
          }
        }
      }
    }
    return
  }
}


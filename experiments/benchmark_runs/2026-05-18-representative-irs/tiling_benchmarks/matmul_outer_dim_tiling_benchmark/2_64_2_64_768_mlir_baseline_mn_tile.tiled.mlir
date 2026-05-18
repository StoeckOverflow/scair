#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 64, s0)>
module {
  func.func @matmul_outer_dim_tiling(%arg0: index, %arg1: index, %arg2: index, %arg3: index, %arg4: index, %arg5: memref<?xf32>, %arg6: memref<?xf32>, %arg7: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %cst = arith.constant 0.000000e+00 : f32
    %0 = arith.muli %arg0, %arg1 : index
    %1 = arith.muli %arg2, %arg3 : index
    %reinterpret_cast = memref.reinterpret_cast %arg5 to offset: [%c0], sizes: [%0, %arg4], strides: [%arg4, %c1] : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %reinterpret_cast_0 = memref.reinterpret_cast %arg6 to offset: [%c0], sizes: [%arg4, %1], strides: [%1, %c1] : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %reinterpret_cast_1 = memref.reinterpret_cast %arg7 to offset: [%c0], sizes: [%0, %1], strides: [%1, %c1] : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    affine.for %arg8 = 0 to %0 step 64 {
      affine.for %arg9 = 0 to %1 step 64 {
        affine.for %arg10 = #map(%arg8) to min #map1(%arg8)[%0] {
          affine.for %arg11 = #map(%arg9) to min #map1(%arg9)[%1] {
            %2 = affine.for %arg12 = 0 to %arg4 iter_args(%arg13 = %cst) -> (f32) {
              %3 = memref.load %reinterpret_cast[%arg10, %arg12] : memref<?x?xf32, strided<[?, ?], offset: ?>>
              %4 = memref.load %reinterpret_cast_0[%arg12, %arg11] : memref<?x?xf32, strided<[?, ?], offset: ?>>
              %5 = arith.mulf %3, %4 : f32
              %6 = arith.addf %arg13, %5 : f32
              affine.yield %6 : f32
            }
            memref.store %2, %reinterpret_cast_1[%arg10, %arg11] : memref<?x?xf32, strided<[?, ?], offset: ?>>
          }
        }
      }
    }
    return
  }
}


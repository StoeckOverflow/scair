#map = affine_map<(d0) -> (d0)>
#map1 = affine_map<(d0)[s0] -> (d0 + 128, s0)>
module {
  func.func @matmul_tiling(%arg0: index, %arg1: index, %arg2: index, %arg3: index, %arg4: memref<?xf32>, %arg5: memref<?xf32>, %arg6: memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %cst = arith.constant 0.000000e+00 : f32
    %0 = arith.muli %arg2, %arg3 : index
    %reinterpret_cast = memref.reinterpret_cast %arg4 to offset: [%c0], sizes: [%arg0, %0], strides: [%0, %c1] : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %reinterpret_cast_0 = memref.reinterpret_cast %arg5 to offset: [%c0], sizes: [%0, %arg1], strides: [%arg1, %c1] : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    %reinterpret_cast_1 = memref.reinterpret_cast %arg6 to offset: [%c0], sizes: [%arg0, %arg1], strides: [%arg1, %c1] : memref<?xf32> to memref<?x?xf32, strided<[?, ?], offset: ?>>
    affine.for %arg7 = 0 to %arg0 step 128 {
      affine.for %arg8 = 0 to %arg1 step 128 {
        affine.for %arg9 = #map(%arg7) to min #map1(%arg7)[%arg0] {
          affine.for %arg10 = #map(%arg8) to min #map1(%arg8)[%arg1] {
            %1 = affine.for %arg11 = 0 to %0 iter_args(%arg12 = %cst) -> (f32) {
              %2 = memref.load %reinterpret_cast[%arg9, %arg11] : memref<?x?xf32, strided<[?, ?], offset: ?>>
              %3 = memref.load %reinterpret_cast_0[%arg11, %arg10] : memref<?x?xf32, strided<[?, ?], offset: ?>>
              %4 = arith.mulf %2, %3 : f32
              %5 = arith.addf %arg12, %4 : f32
              affine.yield %5 : f32
            }
            memref.store %1, %reinterpret_cast_1[%arg9, %arg10] : memref<?x?xf32, strided<[?, ?], offset: ?>>
          }
        }
      }
    }
    return
  }
}

